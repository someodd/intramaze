{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Websocket chat system.

System for allowing ephemeral chat in each room.

This may change to use REDIS or something of the sort, because as of now
everything is kept in memory.

Please see LICENSE-WS.

Will be changed to use authentication and tapping into database. This will
require a separate/another version of runDB/runDbWithCatcher to not be in the
Scotty monad transformer context by default.
-}
module Interwebz.ChatWebSocket where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Char (isPunctuation, isSpace)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.UUID as UUID
import qualified Database.Persist as DB
import qualified Network.WebSockets as WS

import Interwebz.Config (Config (..), getConfig)
import Interwebz.Middle (runDbNoScotty)
import Interwebz.Models (Key (RoomKey), RowUUID (RowUUID))

-- | Represent a client by their username, which room they are in, and a websocket connection.
type Client = (Text, RowUUID, WS.Connection)

{- | The state kept on the server is simply a list of connected clients, managed
by some utility functions.

May be changed to a `HashMap` in the future, or may use something like Redis in
the future.
-}
type ServerState = [Client]

-- | A utility function to give back the username. I chose for this to be more specific.
username :: Client -> Text
username (t, _, _) = t

-- | Utility function which gets a Client's room.
client'sRoom :: Client -> RowUUID
client'sRoom (_, rowUuid, _) = rowUuid

-- | Create a new, initial state.
newServerState :: ServerState
newServerState = []

-- | Get the number of active clients.
numClients :: ServerState -> Int
numClients = length

-- | Check if a user already exists (based on username).
clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== username client) . username)

{- | Add a client.

This does not check if the client already exists, you should do
this yourself using `clientExists`.
-}
addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= username client) . username)

-- | Remove clients not in specified room.
removeNotInRoom :: RowUUID -> ServerState -> ServerState
removeNotInRoom rowUuid = filter ((== rowUuid) . client'sRoom)

-- NOTE: could be abstracted to just updateClient which replaces the existing client with a new one?

-- | Update the room the client is in.
updateClientRoom :: RowUUID -> Client -> ServerState -> ServerState
updateClientRoom rowUuid client serverState =
    let (user, _, connection) = client
        newClient = (user, rowUuid, connection)
     in addClient newClient $ removeClient client serverState

-- all this won't work because... do i need a different connection depending on the room i'm in?

-- | Send a message to all clients, and log it on stdout:
broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, _, conn) -> WS.sendTextData conn message

roomBroadcast :: RowUUID -> Text -> ServerState -> IO ()
roomBroadcast rowUuid message clients = do
    T.putStrLn message
    forM_ (removeNotInRoom rowUuid clients) $ \(_, _, conn) -> WS.sendTextData conn message

{- | Create a preliminary Client from the first message (no validation) or fail.

This handles the setup of a user for the purpose of communicating to a certain
room. It's basically like a protocol.
-}
parseFirstMessageOrFail :: Text -> WS.Connection -> ServerState -> IO (Either Text Client)
parseFirstMessageOrFail msg conn clients = do
    case getAnnounceRowUUID msg >>= \uuid -> Just (getAnnounceName msg, uuid, conn) of
        Just client -> do
            -- This could probably be less hacky. Doing the line below in order
            -- to avoid putting it in the conditional because of IO monad.
            roomByThisUuidExists <- roomExists $ client'sRoom client
            case msg of
                -- Check that the first message has the right format (Hi I am username in someroom!):
                _
                    | not (isAnnounceCorrect msg) ->
                        pure $ Left "Wrong announcement"
                    -- Check the validity of the username:
                    | any
                        ($ username client)
                        [T.null, T.any isPunctuation, T.any isSpace] ->
                        pure $ Left "Name cannot contain punctuation or whitespace, and cannot be empty"
                    -- Check the validity of the room UUID
                    | any
                        ($ getAnnounceRoom msg)
                        [not . isValidUuid . T.unpack, not . const roomByThisUuidExists . T.unpack] ->
                        pure $ Left "Invalid room UUID"
                    -- Check that the given username is not already taken:
                    | clientExists client clients ->
                        pure $ Left "User already exists"
                    | otherwise -> pure $ Right client
        Nothing -> pure $ Left "Invalid room UUID!"
  where
    isAnnounceCorrect s = T.take 14 (T.take 7 s <> T.drop 43 s) == "Hello, ! I am "
    getAnnounceName = T.drop 50
    getAnnounceRoom = T.take 36 . T.drop 7
    getAnnounceRowUUID s = RowUUID <$> (UUID.fromString . T.unpack . getAnnounceRoom) s
    isValidUuid = isJust . UUID.fromString

    roomExists :: RowUUID -> IO Bool
    roomExists i = do
        config <- getConfig
        _ <- pure $ pool config
        m <- runDbNoScotty (DB.get (RoomKey i))
        case m of
            Nothing -> pure False
            Just _ -> pure True

{- | Our main application has this type...

Note that `WS.ServerApp` is nothing but a type synonym for
`WS.PendingConnection -> IO ()`.

Our application starts by accepting the connection. In a more realistic
application, you probably want to check the path and headers provided by the
pending request.

We also fork a pinging thread in the background. This will ensure the connection
stays alive on some browsers.
-}
application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
        -- When a client is succesfully connected, we read the first message. This should
        -- be in the format of "Hello, roomuuid! I am Jasper", where Jasper is the requested username
        -- and roomuuid is the UUID of the room to join.
        (msg :: Text) <- WS.receiveData conn
        clients <- readMVar state
        firstMessageProduct <- parseFirstMessageOrFail msg conn clients
        case firstMessageProduct of
            Left err -> WS.sendTextData conn err
            -- All is right! We're going to allow the client, but for safety reasons we *first*
            -- setup a `disconnect` function that will be run when the connection is closed.
            Right validClient -> flip finally disconnect $ do
                -- We send a "Welcome!", according to our own little protocol. We add the client to
                -- the list and broadcast the fact that he has joined. Then, we give control to the
                -- 'talk' function.
                modifyMVar_ state $ \s -> do
                    let s' = addClient validClient s
                    WS.sendTextData conn $
                        "Welcome! Users: "
                            <> T.intercalate ", " (map username $ filter ((client'sRoom validClient ==) . client'sRoom) s)
                    -- should only broadcast to people in the same room as client!
                    roomBroadcast (client'sRoom validClient) (username validClient <> " joined") s'
                    return s'
                talk validClient state
              where
                disconnect = do
                    -- Remove client and return new state
                    s <- modifyMVar state $ \s ->
                        let s' = removeClient validClient s in return (s', s')
                    broadcast (username validClient <> " disconnected") s

{- | The talk function continues to read messages from a single client until he
disconnects. All messages are broadcasted to the other clients.
-}
talk :: Client -> MVar ServerState -> IO ()
talk (user, rowUuid, conn) state = forever $ do
    msg <- WS.receiveData conn
    readMVar state
        >>= roomBroadcast
            rowUuid
            (user `mappend` ": " `mappend` msg)

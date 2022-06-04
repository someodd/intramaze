{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This started of as a copy of the websocket chat demo.
module IntraMaze.ChatWebSocket where

import Data.Char (isPunctuation, isSpace)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS
import IntraMaze.Models (RowUUID (RowUUID))
import qualified Data.UUID as UUID
import Data.Maybe (isJust)
--import IntraMaze.Middle
--import qualified Database.Persist as DB


-- FIXME: just make RowUUID a Text and make a type RoomUuidText = Text. it's just cleaner that way!
-- then you can just make the fromString uuid thingy one of the verification steps.
-- | Represent a client by their username, which room they are in, and a websocket connection.
type Client = (Text, RowUUID, WS.Connection)


-- TODO/FIXME: should maybe be a map, mapping room id to client. could use redis or something in future.
-- | The state kept on the server is simply a list of connected clients, managed
-- by some utility functions.
type ServerState = [Client]


-- TODO: rename to client'sUsername?
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


-- | Add a client (this does not check if the client already exists, you should do
-- this yourself using `clientExists`).
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


{-
The main function first creates a new state for the server, then spawns the
actual server. For this purpose, we use the simple server provided by
`WS.runServer`.
main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state
-}


-- | Create a preliminary Client from the first message (no validation) or fail.
parseFirstMessageOrFail :: Text -> WS.Connection -> ServerState -> Either Text Client
parseFirstMessageOrFail msg conn clients = do
    client <- case getAnnounceRowUUID msg >>= \uuid -> Just (getAnnounceName msg, uuid, conn) of
        Just client -> Right client
        Nothing -> Left "Invalid room UUID!"
    -- FIXME: intro message needs to also specify which room you're in? also how to update which room in?
    case msg of
            -- Check that the first message has the right format (Hi I am username in someroom!):
        _   | not (isAnnounceCorrect msg) ->
                Left "Wrong announcement"
            -- Check the validity of the username:
            | any ($ username client)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    Left "Name cannot contain punctuation or whitespace, and cannot be empty"
            -- Check the validity of the room UUID
            | any ($ getAnnounceRoom msg)
                [not . isValidUuid . T.unpack] -> -- TODO:, not (roomExists $ client'sRoom client)] ->
                    Left "Invalid room UUID"
            -- Check that the given username is not already taken:
            | clientExists client clients ->
                Left "User already exists"
            | otherwise -> Right client
   where
    isAnnounceCorrect s = T.take 14 (T.take 7 s <> T.drop 43 s) == "Hello, ! I am "
    getAnnounceName = T.drop 50
    getAnnounceRoom = T.take 36 . T.drop 7
    getAnnounceRowUUID s = RowUUID <$> (UUID.fromString . T.unpack . getAnnounceRoom) s
    isValidUuid = isJust . UUID.fromString

    {- FIXME: implementing this currently would be very difficult because of the monad tranformer stuff related to db... save for later!
    will need to make my own readert and run it?
    roomExists :: RowUUID -> IO ConfigM Bool
    roomExists i = do
        m <- runDB (DB.get (RoomKey i))
        case m of
            Nothing -> pure False
            Just _ -> pure True
    -}


-- | Our main application has this type...
--
-- Note that `WS.ServerApp` is nothing but a type synonym for
-- `WS.PendingConnection -> IO ()`.
--
-- Our application starts by accepting the connection. In a more realistic
-- application, you probably want to check the path and headers provided by the
-- pending request.
--
-- We also fork a pinging thread in the background. This will ensure the connection
-- stays alive on some browsers.
application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
        -- When a client is succesfully connected, we read the first message. This should
        -- be in the format of "Hello, roomuuid! I am Jasper", where Jasper is the requested username
        -- and roomuuid is the UUID of the room to join.
        (msg :: Text) <- WS.receiveData conn
        clients <- readMVar state
        case parseFirstMessageOrFail msg conn clients of
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
                        "Welcome! Users: " <>
                        T.intercalate ", " (map username $ filter ((client'sRoom validClient ==) . client'sRoom) s)
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


-- | The talk function continues to read messages from a single client until he
-- disconnects. All messages are broadcasted to the other clients.
talk :: Client -> MVar ServerState -> IO ()
talk (user, rowUuid, conn) state = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= roomBroadcast
        rowUuid
        (user `mappend` ": " `mappend` msg)

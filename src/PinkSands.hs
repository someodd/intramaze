-- | REST API and web socket server converge here.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PinkSands where

-- FIXME: qualified imports?
import Network.Wai.Middleware.Cors
import Protolude (putText, threadDelay, forever, forkIO)
import qualified Network.WebSockets as WS
import Data.ByteString.UTF8 as BS (toString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Class (MonadTrans)
import Data.Aeson (Value (Null), (.=), object)
import Data.Default (def)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text.Lazy (Text)
import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import PinkSands.Models (Room (..), migrateAll, Key(..), Portal, RoomUUID, EntityField(..), PortalId)
import qualified PinkSands.ChatWebSocket as CWS (application, newServerState)
import Control.Concurrent (MVar, newMVar)
import Network.HTTP.Types.Status (created201, internalServerError500,
    notFound404, status204, badRequest400)
import Network.Wai (Middleware, Response)
import Network.Wai.Parse (FileInfo(..))
import Network.Wai.Handler.Warp (Settings, defaultSettings,
    setFdCacheDuration, setPort, runSettings, getPort)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import System.Environment (lookupEnv)
import Web.Heroku (parseDatabaseUrl)
import Web.Scotty.Trans (ActionT, Options, ScottyT, defaultHandler,
    get, json, jsonData, middleware, notFound, param, post,
    settings, showError, status, verbose, put, delete, files, patch, scottyAppT)
import PinkSands.Static (createRoomImage, createNewRoom, setupEssentials)
import Database.Persist (Entity (entityVal), Filter (Filter), FilterValue (..))
--import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import Control.Monad (when)
import Network.WebSockets.Connection (pingThread)
import PinkSands.ChatWebSocket (ServerState)
import PinkSands.Middle
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HashMap
import Data.Either (rights)
import Data.Either (lefts)


main :: IO ()
main = do
  c <- getConfig
  migrateSchema c
  runApplication c


migrateSchema :: Config -> IO ()
migrateSchema c =
  liftIO $ flip DB.runSqlPersistMPool (pool c) $ DB.runMigration migrateAll


getConfig :: IO Config
getConfig = do
  e <- getEnvironment
  p <- getPool e
  return Config
    { environment = e
    , pool = p
    }


getEnvironment :: IO Environment
getEnvironment = do
  m <- lookupEnv "SCOTTY_ENV"
  let e = case m of
        Nothing -> Development
        Just s -> read s
  return e


getPool :: Environment -> IO DB.ConnectionPool
getPool e = do
  s <- getConnectionString e
  let n = getConnectionSize e
  case e of
    Development -> runStdoutLoggingT (DB.createPostgresqlPool s n)
    Production -> runStdoutLoggingT (DB.createPostgresqlPool s n)
    Test -> runNoLoggingT (DB.createPostgresqlPool s n)

                                                                                                                                                                                
getConnectionString :: Environment -> IO DB.ConnectionString
getConnectionString e = do
  m <- lookupEnv "DATABASE_URL"
  let s = case m of
        Nothing -> getDefaultConnectionString e
        Just u -> createConnectionString (parseDatabaseUrl u)
  return s


getDefaultConnectionString :: Environment -> DB.ConnectionString
getDefaultConnectionString Development =
  "host=localhost port=5432 user=postgres dbname=pinksands_development"
getDefaultConnectionString Production =
  "host=localhost port=5432 user=postgres dbname=pinksands_production"
getDefaultConnectionString Test =
  "host=localhost port=5432 user=testpguser password=testpguser dbname=testpgdatabase"


createConnectionString :: [(T.Text, T.Text)] -> DB.ConnectionString
createConnectionString l =
  let f (k, v) = T.concat [k, "=", v]
  in  encodeUtf8 (T.unwords (map f l))


getConnectionSize :: Environment -> Int
getConnectionSize Development = 1
getConnectionSize Production = 8
getConnectionSize Test = 1


runApplication :: Config -> IO ()
runApplication c = do
  o <- getOptions (environment c)
  let r m = runReaderT (runConfigM m) c
      app = application c
  state <- newMVar CWS.newServerState
  scottyOptsT' state o r app


-- CUSTOM FOR WS
-- thanks in part due to https://gist.github.com/andrevdm/9560b5e31933391694811bf22e25c312
-- | Run a scotty application using the warp server, passing extra options.
-- NB: scottyOpts opts === scottyOptsT opts id
scottyOptsT' :: (Monad m, MonadIO n)
            => MVar ServerState
            -> Options
            -> (m Response -> IO Response) -- ^ Run monad 'm' into 'IO', called at each action.
            -> ScottyT e m ()
            -> n ()
scottyOptsT' state opts runActionToIO s = do
    when (verbose opts > 0) $
        -- nam eissues
        liftIO $ putStrLn $ "Setting phasers to stun... (port " ++ show (getPort (settings opts)) ++ ") (ctrl-c to quit)"
    -- somehow on the left side of this bind operation you can use websocketsOr because the bind
    -- will get Application!
    liftIO . runSettings (settings opts) . WaiWs.websocketsOr WS.defaultConnectionOptions (CWS.application state) =<< scottyAppT runActionToIO s


getOptions :: Environment -> IO Options
getOptions e = do
  s <- getSettings e
  return def
    { settings = s
    , verbose = case e of
      Development -> 1
      Production -> 0
      Test -> 0
    }

-- should i set cors here?
getSettings :: Environment -> IO Settings
getSettings e = do
  let s = defaultSettings
      s' = case e of
        Development -> setFdCacheDuration 0 s
        Production -> s
        Test -> s
  m <- getPort'
  let s'' = case m of
        Nothing -> s'
        Just p -> setPort p s'
  return s''


getPort' :: IO (Maybe Int)
getPort' = do
  m <- lookupEnv "PORT"
  let p = case m of
        Nothing -> Nothing
        Just s -> Just (read s)
  return p


type Error = Text


application :: Config -> ScottyT Error ConfigM ()
application c = do
  let e = environment c
  middleware (loggingM e)
  -- FIXME: this doesn't belong here and shouldn't always be enabled. delete this! only should be enabled on develop mode
  middleware $ cors (const . Just $ simpleCorsResourcePolicy {corsMethods=["GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"]})
  defaultHandler (defaultH e)
  -- Rooms
  -- TODO: patch room? or will i always be doing put...
  get "/rooms" getRoomsA
  post "/rooms" postRoomsA
  get "/rooms/:id" getRoomA
  patch "/rooms/:id" patchRoomA
  get "/rooms/search" getRoomSearchA
  get "/rooms/:id/generate" getRoomGenerateA
  get "/generate" getGenerateEverythingA
  put "/rooms/:id" putRoomA
  post "/rooms/:id/image" postRoomsImageA
  delete "/rooms/:id" deleteRoomA
  -- Portals.
  -- FIXME: the endpoint should look like /rooms/:id/portals, but it's not because the JSON serializer
  -- gets confused because it expects `belongsTo` so we just made the endpoint `/portals`!
  post "/portals" postPortalsA
  delete "/portals/:id" deletePortalsA
  get "/rooms/:id/portals" getRoomsPortalsA
  get "/portals" getPortalsA
  -- rest...
  notFound notFoundA


loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production = logStdout
loggingM Test = id


type Action = ActionT Error ConfigM ()

-- | Something something!
defaultH :: Environment -> Error -> Action
defaultH e x = do
  status internalServerError500
  let o = case e of
        Development -> object ["error" .= showError x]
        Production -> Null
        Test -> object ["error" .= showError x]
  json o


getRoomsA :: Action
getRoomsA = do
  ts <- runDB (DB.selectList [] [])
  json (ts :: [DB.Entity Room])


-- | REST endpoint for creating a room.
--
-- Creates the static files and directory for the room.
postRoomsA :: Action
postRoomsA = do
  t <- jsonData
  -- could use insert instead of insert_ to get the key back and we can give the key as a response or add it to
  -- jsonData?
  (RoomKey uuid) <- runDB (DB.insert t)
  -- FIXME: should we be pointing the database to this? it'd make sense.
  filePath <- liftIO $ createNewRoom uuid t []
  status created201
  json (filePath, uuid)


-- FIXME: messy because you're not used to dealing with transformers and monads this much
-- | Static generation of a room based off the RoomUUID.
--
-- The return value is "Maybe FilePath" instead of simply "FilePath," because
-- no room by the supplied `uuid` may exist!
--
-- Helper function.
generateRoom
  :: (MonadTrans t, MonadIO (t ConfigM))
  => RoomUUID
  -> t ConfigM (Maybe FilePath)
generateRoom uuid = do
  roomMaybe <- runDB $ DB.get (RoomKey uuid)
  portals <- runDB $ DB.selectList [PortalBelongsTo DB.==. RoomKey uuid] []
  case roomMaybe >>= \room -> Just $ createNewRoom uuid room [entityVal portal | portal <- portals] of
    Nothing -> pure Nothing
    Just roomPath -> do
      path <- liftIO roomPath
      pure $ Just path


-- FIXME: POST instead?
-- | REST endpoint for calling for the static HTML file of a room.
getRoomGenerateA :: Action
getRoomGenerateA = do
  (i :: RoomUUID) <- param "id"
  roomPathMaybe <- generateRoom i
  case roomPathMaybe of
    Nothing ->
      notFoundA
    Just path -> do
      status created201 
      json path


-- FIXME: should have separate thing for just regenerating rooms nad not entire site?
-- TODO: generate just the essentials as its own command...
-- FIXME: POST instead? which HTTP verb?
-- | REST endpoint for creating the static HTML files for ALL the rooms/portals/entire site.
--
-- Also setup essential files.
getGenerateEverythingA :: Action
getGenerateEverythingA = do
  (roomKeySelection :: [DB.Key Room]) <- runDB (DB.selectKeysList [] [])
  paths <- traverse (\(RoomKey uuid) -> generateRoom uuid) roomKeySelection
  builtStaticPaths <- liftIO setupEssentials
  status created201 
  json $ [p | Just p <- paths] ++ builtStaticPaths


-- FIXME: needs to use directory for room ID...
-- | Endpoint for uploading the room image (for the specified room).
--
-- Expects `multipart/form-data` as encoding type.
postRoomsImageA :: Action
postRoomsImageA = do
  (i :: RoomUUID) <- param "id"
  files' <- files
  let fileInfo = case files' of
                   ("image", fileInfo'):_ -> fileInfo'
                   _ -> error "nothing"
      imageFileName = fileName fileInfo
  runDB (DB.update (RoomKey i) [RoomBgFileName DB.=. Just (decodeUtf8 imageFileName)])
  pathToImage <- liftIO $ createRoomImage i (fileContent fileInfo) (toString imageFileName)
  status created201
  json pathToImage


getRoomA :: Action
getRoomA = do
  -- can fix this by importing and creating an instance of import Web.Scotty.Action (Parsable) for RoomUUID
  i <- param "id"
  --m <- runDB (DB.getBy (UniqueRoomID i))
  --m <- runDB (DB.get (toKey i))
  m <- runDB (DB.get (RoomKey i))
  case m of
    Nothing -> notFoundA
    Just t -> json (t :: Room)


-- FIXME: also by title?
-- | Search/query rooms by description partial match.
getRoomSearchA :: Action
getRoomSearchA = do
  (description :: T.Text) <- param "description"
  --m <- runDB (DB.selectList [(RoomDescription) DB.==. (RoomKey i)] [])
  -- FIXME: this string substitution is VERY BAD. i also thing there's  abetter way to do this in 0.6.
  m <- runDB $ DB.selectList [Filter RoomDescription (FilterValue . Just $ "%" <> description <> "%") (DB.BackendSpecificFilter "ILIKE")] []
  json (m :: [DB.Entity Room])


-- TODO: patchRoomA
-- FIXME why does it expect the primary key (custom) in the json even tho it's defined in id?
putRoomA :: Action
putRoomA = do
  i <- param "id"
  t <- jsonData
  runDB (DB.repsert (RoomKey i) t)
  json (t :: Room)


-- FIXME
-- | update room records. currently only supports updating the title and description.
-- FIXME: needs to be fixed because it should be able to update many fields at once! this is terrible.
patchRoomA :: Action
patchRoomA = do
  i <- param "id"
  (updateObject :: A.Object) <- jsonData
  -- FIXME: this is the worst way of checking if data is valid and
  -- updating based off this... use the framework. at least have validator
  -- function that does all this automatically for any field should be easy.
  -- i want to be able to set the type to be variable but that may not be
  -- possible?
  let
    descOrFailure =
      case HashMap.lookup "description" updateObject of
        Just (A.String description) ->
          Right [RoomDescription DB.=. Just description]
        Just _ -> Left "description needs to be a string."
        Nothing -> Right []
    titleOrFailure =
      case HashMap.lookup "title" updateObject of
        Just (A.String title) ->
         Right [RoomTitle DB.=. Just title]
        Just _ -> Left "title needs to be a string."
        Nothing -> Right []
    toMatch = rights [descOrFailure, titleOrFailure]
    failures = lefts [descOrFailure, titleOrFailure]
  case failures of
    [] -> do
      -- FIXME: what if neither desc or title?
      -- FIXME: might not become a Room!
      runDB (DB.update (RoomKey i) $ concat toMatch)
      -- fixme: needs status! and return json!
      json Null
    failureList -> do
      status badRequest400 
      -- FIXME: error should be an object. {error: error message} or something...
      -- could even put status code there.
      json $ "error(s): " <> concat failureList


{-
-- aesonType == A.String and valuewrapper == Just
updateConstructor :: Text -> A.Object -> (* -> *) -> (* -> *) -> [a]
updateConstructor key updateObject aesonType valueWrapper =
    case HashMap.lookup key updateObject of
    Just (aesonType value) ->
      [RoomDescription DB.=. valueWrapper value]
    Just _ -> do
        status badRequest400
        -- FIXME: error should be an object. {error: error message} or something...
        -- could even put status code there.
        json $ "error: " <> key <> " needs to be a " <> show aesonType
    Nothing -> pure pure pure
-}


-- | Delete a room by the specified room UUID and all of its portals.
deleteRoomA :: Action
deleteRoomA = do
  (i :: RoomUUID) <- param "id"
  runDB (DB.delete (RoomKey i))
  runDB (DB.deleteWhere [PortalBelongsTo DB.==. RoomKey i])
  json Null


getPortalsA :: Action
getPortalsA = do
  ts <- runDB (DB.selectList [] [])
  json (ts :: [DB.Entity Portal])


-- NOTE: I don't understand the fromIntegral/Integer bit of this function.
-- Doesn't it already know it's an integer, since that's in the function's signature?
toKey :: DB.ToBackendKey DB.SqlBackend a => Integer -> DB.Key a
toKey i = DB.toSqlKey (fromIntegral (i :: Integer))


getRoomsPortalsA :: Action
getRoomsPortalsA = do
  -- can fix this by importing and creating an instance of import Web.Scotty.Action (Parsable) for RoomUUID
  (i :: RoomUUID) <- param "id"
  --m <- runDB (DB.getBy (UniqueRoomID i))
  --m <- runDB (DB.get (toKey i))
  m <- runDB (DB.selectList [(PortalBelongsTo) DB.==. (RoomKey i)] [])
  json (m :: [DB.Entity Portal])


deletePortalsA :: Action
deletePortalsA = do
  i <- param "id"
  runDB (DB.delete (toKey i :: PortalId))
  status status204
  json Null


-- NOTE: won't this have to deal with multiple portals? how will we handle adding mroe than one... patch? i guess you can keep posting them?
postPortalsA :: Action
postPortalsA = do
  t <- jsonData
  -- could use insert instead of insert_ to get the key back and we can give the key as a response or add it to
  -- jsonData?
  runDB (DB.insert_ t)
  status created201
  json (t :: Portal)


notFoundA :: Action
notFoundA = do
  status notFound404
  json Null


--


-- old wsapp
wsapp :: WS.ServerApp
wsapp pending = do
  putText "ws connected"
  conn <- WS.acceptRequest pending
  --WS.forkPingThread conn 30
  -- should use withPingThread instead

  _ <- forkIO $ pingThread conn 30 (return ())

  (msg :: T.Text) <- WS.receiveData conn
  WS.sendTextData conn $ ("initial> ") <> msg

  forever $ do
    WS.sendTextData conn $ ("loop data" :: T.Text)
    threadDelay $ 1 * 1000000
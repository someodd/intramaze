-- | REST API and web socket server converge here.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module PinkSands where

-- FIXME: start moving things to PinkSands.Routes or PinkSands.Actions?
-- FIXME: qualified imports?
import Network.Wai.Middleware.Cors
import Protolude (putText, threadDelay, forever, forkIO)
import qualified Network.WebSockets as WS
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (Value (Null), (.=), object)
import Data.Default (def)
import qualified Data.Text as T
--import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Database.Persist.Postgresql as DB
import PinkSands.Models (migrateAll)
import qualified PinkSands.ChatWebSocket as CWS (application, newServerState)
import Control.Concurrent (MVar, newMVar)
import Network.HTTP.Types.Status (internalServerError500)
import Network.Wai (Middleware, Response)
import Network.Wai.Handler.Warp (Settings, defaultSettings,
    setFdCacheDuration, setPort, runSettings, getPort)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import System.Environment (lookupEnv)
import Web.Heroku (parseDatabaseUrl)
import Web.Scotty.Trans (Options, ScottyT, defaultHandler,
    get, json, middleware, notFound, post,
    settings, showError, status, verbose, put, delete, patch, scottyAppT)
--import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import Control.Monad (when)
import Network.WebSockets.Connection (pingThread)
import PinkSands.ChatWebSocket (ServerState)
import PinkSands.Middle
import Data.Text.Encoding as TSE
import qualified PinkSands.Actions as Actions
import PinkSands.Config
import PinkSands.Static (setupEssentials)


main :: IO ()
main = do
  _ <- setupEssentials  -- FIXME: this should only be done with a flag.
  c <- getConfig
  migrateSchema c
  runApplication c


migrateSchema :: Config -> IO ()
migrateSchema c = do
  liftIO $ flip DB.runSqlPersistMPool (pool c) $ DB.runMigration migrateAll
  liftIO $ flip DB.runSqlPersistMPool (pool c) $ DB.runMigration $ DB.migrateEnableExtension "pgcrypto"


getConfig :: IO Config
getConfig = do
  e <- getEnvironment
  aec <- getAppEnvConfig
  p <- getPool e
  return Config
    { environment = e
    , appEnvConfig = aec
    , pool = p
    }


-- FIXME: redundant considering `appEnvConfig`.
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


application :: Config -> ScottyT Error ConfigM ()
application c = do
  let e = environment c
  middleware (loggingM e)
  -- FIXME: this doesn't belong here and shouldn't always be enabled. delete this! only should be enabled on develop mode
  middleware $ cors (const . Just $ simpleCorsResourcePolicy {corsMethods=["GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"], corsRequestHeaders=["Authorization", "Content-Type"]})
  defaultHandler (defaultH e)
  -- Rooms
  -- TODO: patch room? or will i always be doing put...
  get "/rooms" Actions.getRoomsA
  post "/rooms" Actions.postRoomsA
  get "/rooms/:id" Actions.getRoomA
  patch "/rooms/:id" Actions.patchRoomA
  get "/rooms/search" Actions.getRoomSearchA
  get "/rooms/:id/generate" Actions.getRoomGenerateA
  get "/generate" Actions.getGenerateEverythingA
  put "/rooms/:id" Actions.putRoomA
  post "/rooms/:id/image" Actions.postRoomsImageA
  delete "/rooms/:id" Actions.deleteRoomA
  -- Portals.
  -- FIXME: the endpoint should look like /rooms/:id/portals, but it's not because the JSON serializer
  -- gets confused because it expects `belongsTo` so we just made the endpoint `/portals`!
  post "/portals" Actions.postPortalsA
  delete "/portals/:id" Actions.deletePortalsA
  get "/rooms/:id/portals" Actions.getRoomsPortalsA
  get "/portals" Actions.getPortalsA

  -- user authorization
  get "/users/whoami" Actions.getWhoamiA 
  post "/users" Actions.postUserA
  post "/users/login" Actions.postUserLoginA
  -- FIXME: this is a test
  post "/testrequire" Actions.postTestRequire

  -- rest...
  notFound Actions.notFoundA


loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production = logStdout
loggingM Test = id


-- | Something something!
defaultH :: Environment -> Error -> Actions.Action
defaultH e x = do
  status internalServerError500
  let o = case e of
        Development -> object ["error" .= showError x]
        Production -> Null
        Test -> object ["error" .= showError x]
  json o


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
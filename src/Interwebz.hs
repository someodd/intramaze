-- | REST API and web socket server converge here. The scotty app...
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Interwebz where

-- FIXME: start moving things to Interwebz.Routes or Interwebz.Actions?
-- FIXME: qualified imports?
import Network.Wai.Middleware.Cors
import Protolude (putText, threadDelay, forever, forkIO)
import qualified Network.WebSockets as WS
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (Value (Null), (.=), object)
import qualified Data.Text as T
--import Data.Text.Encoding (encodeUtf8, decodeUtf8)  
import qualified Database.Persist.Postgresql as DB
import Interwebz.Models (migrateAll)
import qualified Interwebz.ChatWebSocket as CWS (application, newServerState)
import Control.Concurrent (MVar, newMVar)
import Network.HTTP.Types.Status (internalServerError500)
import Network.Wai (Middleware, Response, Application)
import Network.Wai.Handler.Warp (runSettings, getPort)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Web.Scotty.Trans (Options, ActionT, ScottyT, defaultHandler,
    get, json, middleware, notFound, post,
    settings, showError, status, verbose, put, delete, patch, scottyAppT)
--import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import Control.Monad (when)
import Network.WebSockets.Connection (pingThread)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Interwebz.ChatWebSocket (ServerState)
import qualified Interwebz.Middle as Middle
import qualified Interwebz.Actions as Actions
import Interwebz.Config
import Interwebz.Static (setupEssentials)
import Interwebz.Database (createDefaultAdmin)
import qualified Interwebz.ActionHelpers as ActionHelpers
import qualified Web.Scotty as Web.Scotty.Internal.Types
import qualified Web.Scotty.Internal.Types
import Data.Text.Lazy (pack)


appToIO :: Config -> ScottyT Middle.ApiError ConfigM () -> IO Application
appToIO c app = scottyAppT (flip runReaderT c . runConfigM) app
                           
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


runApplication :: Config -> IO ()
runApplication c = do
  o <- getOptions (environment c)
  let r m = runReaderT (runConfigM m) c
      app = application c
  state <- newMVar CWS.newServerState
  scottyOptsT' state o r app

-- | Run a scotty application using the warp server, passing extra options.
--
-- Made custom for web sockets to run along side the REST server.
--
-- Thanks in part due to https://gist.github.com/andrevdm/9560b5e31933391694811bf22e25c312
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


-- | Stuff to do the first time the application is ran, such as initializing the database.
initialize :: ActionT Middle.ApiError ConfigM ()
initialize = do
  createDefaultAdmin


-- | Prefixes a route with the /api/v[n] prefix
--api :: (Web.Scotty.Internal.Types.RoutePattern -> ActionT Middle.ApiError ConfigM () -> ScottyT Middle.ApiError ConfigM ()) -> Web.Scotty.Internal.Types.RoutePattern -> ScottyT Middle.ApiError ConfigM ()
api :: (Web.Scotty.Internal.Types.RoutePattern -> t1 -> t2) -> [Char] -> t1 -> t2
api method route action = method (Web.Scotty.Internal.Types.Capture . pack $ "/api/v" ++ show restApiVersionMajor ++ route :: Web.Scotty.Internal.Types.RoutePattern) action


application :: Config -> ScottyT Middle.ApiError ConfigM ()
application c = do
  let e = environment c

  -- Only serve static files if testing server.
  if e == Test
    then middleware $ staticPolicy $ addBase "built/"
    else middleware id  -- Is this inefficient?

  middleware (loggingM e)
  -- FIXME: this doesn't belong here and shouldn't always be enabled. delete this! only should be enabled on develop mode
  middleware $ cors (const . Just $ simpleCorsResourcePolicy {corsMethods=["GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"], corsRequestHeaders=["Authorization", "Content-Type"]})
  defaultHandler (defaultH e)
  -- Rooms
  -- TODO: patch room? or will i always be doing put...
  api get "/rooms" Actions.getRoomsA  -- Get ALL? Should be put behind root perms FIXME (too intensive)
  api post "/rooms" Actions.postRoomsA
  api get "/rooms/generate" Actions.getRoomsGenerateAll
  api get "/rooms/:id" Actions.getRoomA
  api patch "/rooms/:id" Actions.patchRoomA
  api get "/rooms/search" Actions.getRoomSearchA
  api get "/rooms/:id/generate" Actions.getRoomGenerateA
  api get "/generate" Actions.getGenerateEverythingA
  api put "/rooms/:id" Actions.putRoomA
  api post "/rooms/:id/image" Actions.postRoomsImageA
  api delete "/rooms/:id" Actions.deleteRoomA
  -- Portals.
  -- FIXME: the endpoint should look like /rooms/:id/portals, but it's not because the JSON serializer
  -- gets confused because it expects `belongsTo` so we just made the endpoint `/portals`!
  api post "/portals" Actions.postPortalA
  api delete "/portals/:id" Actions.deletePortalsA
  api get "/rooms/:id/portals" Actions.getRoomsPortalsA
  api get "/portals" Actions.getPortalsA

  -- user authorization
  api get "/users/generate" Actions.getGenerateProfilesA
  api get "/users/:id/generate" Actions.getGenerateSpecificProfileA
  api get "/users/whoami" Actions.getWhoamiA 
  api post "/users" Actions.postUserA
  api post "/users/token" Actions.postUserTokenA

  -- rest...
  notFound ActionHelpers.notFoundA
  --initialize





loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production = logStdout
loggingM Test = id


-- | Something something!
defaultH :: Environment -> Middle.ApiError -> ActionHelpers.Action
defaultH e x = do
  status internalServerError500
  let o = case e of
        Development -> object ["error" .= showError x]
        Production -> Null
        Test -> object ["error" .= showError x]
  json o


-- old wsapp... delete?
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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

{- | Application configuration.

This module not only defines settings directly, but also provides a system for
defining configuration settings, like through environmental variables, and more
configuration-related activities.

Scotty also has a lot of configuration expectations, like how the config being
baked into the Scotty transmonad so the config, pool, can be accessed from any
action.

-}
module Interwebz.Config
    (
    -- * Configuration for Scotty
    -- Configuration specifically for code on the Scotty library level, like the database pool.
      Config(..)
    , ConfigM(..)
    , Environment(..)
    , getConfig
    , getOptions
    -- * Envvar-defined Config System
    -- $envvarConfigSys
    , AppEnvConfig(..)
    , getAppEnvConfig
    , appEnvConfigWhitelist
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import qualified Database.Persist.Postgresql as DB
import Web.Scotty.Internal.Types (Options(..))
import Network.Wai.Handler.Warp (Settings, defaultSettings, setFdCacheDuration, setPort)
import Data.Default (def)
import Data.Text.Encoding (encodeUtf8)
import Web.Heroku (parseDatabaseUrl)
import Control.Monad.Logger (runStdoutLoggingT, NoLoggingT (runNoLoggingT))


{- | The mode the application will run under. Depending on such a mode various
settings will be used (different database, verbosity, etc.).
-}
data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)


{- | The main configuration records. This is used in a complicated manner with a
Scotty monad transformer so the configuration can be read wherever.

Related: `ConfigM`
-}
data Config = Config
  { environment :: Environment -- FIXME: shouldn't this be in appenvconfig?!
  , pool :: DB.ConnectionPool
  , appEnvConfig :: AppEnvConfig
  }


{- | See `Config`. 

-}
newtype ConfigM a = ConfigM
 { runConfigM :: ReaderT Config IO a
 } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)


{- | Environmental variables which allow the app to be easily configured.

Constructed by `getAppEnvConfig`.
-}
data AppEnvConfig = AppEnvConfig
  { confSiteTitle :: Maybe T.Text 
  -- ^ The site title allows the static site builder to incorporate it as a Mustache variable as
  -- {{confSiteTitle}}.
  , confDatabaseUrl :: Maybe String
  -- ^ The connection string used to connect to the PostgreSQL database. The format is
  -- postgres://[user[:password]@][netloc][:port][/dbname][?param1=value1&...]...
  } deriving (Show)


{- | Whitelists record lookup functions belonging to `AppEnvConfig`, mapping
them to a label which shall be used for Mustache {{variable}} substitutions.
Effectively in the form of (key/label, lookup function).

-}
appEnvConfigWhitelist :: [(T.Text, AppEnvConfig -> Maybe T.Text)]
appEnvConfigWhitelist =
    [ ("siteTitle", confSiteTitle)
    ]


{- | This prefix is used before all AppEnvConfig envvars (see `getAppEnvConfig`).

-}
appEnvConfPrefix :: String
appEnvConfPrefix = "SCOTTY_"


{- | Create the raw application configuration records by reading environmental variables.

An example use is using envvars to set
-}
getAppEnvConfig :: IO AppEnvConfig
getAppEnvConfig = do
  AppEnvConfig
    <$> (fmap T.pack <$> lookupEnvPrefixed "SITE_TITLE")
    <*> lookupEnvPrefixed "DATABASE_URL"
 where
  lookupEnvPrefixed :: String -> IO (Maybe String)
  lookupEnvPrefixed s = do
      maybeEnvValue <- lookupEnv . (appEnvConfPrefix ++) $ s
      pure maybeEnvValue



getPool :: Environment -> AppEnvConfig -> IO DB.ConnectionPool
getPool e appEnvConf = do
  let s = getConnectionString e appEnvConf
  let n = getConnectionSize e
  case e of
    Development -> runStdoutLoggingT (DB.createPostgresqlPool s n)
    Production -> runStdoutLoggingT (DB.createPostgresqlPool s n)
    Test -> runNoLoggingT (DB.createPostgresqlPool s n)


getConfig :: IO Config
getConfig = do
  e <- getEnvironment
  aec <- getAppEnvConfig
  p <- getPool e aec
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

                                                                                                                                                                                
getConnectionString :: Environment -> AppEnvConfig -> DB.ConnectionString
getConnectionString e appEnvConf = do
  -- FIXME: should derive from app env config
  --m <- lookupEnv "DATABASE_URL"
  let m = confDatabaseUrl appEnvConf
  case m of
    Nothing -> getDefaultConnectionString e
    Just u -> createConnectionString (parseDatabaseUrl u)
    -- above used to be `createConnectionString (parseDatabaseUrl u)`, but it's broken! maybe pointless? for parsing what we expect for a string... oh i guess it's because it expects a URI?!


-- FIXME: why would this EVER be handy? do not even bother! delete soon!
-- FIXME: I just changed this from localhost to "db"
getDefaultConnectionString :: Environment -> DB.ConnectionString
getDefaultConnectionString Development =
  "host=db port=5432 user=testpguser password=testpguser dbname=postgres"
getDefaultConnectionString Production =
  "host=db port=5432 user=postgres dbname=Interwebz_production"
getDefaultConnectionString Test =
  "host=db port=5432 user=testpguser password=testpguser dbname=postgres"


createConnectionString :: [(T.Text, T.Text)] -> DB.ConnectionString
createConnectionString l =
  let f (k, v) = T.concat [k, "=", v]
  in  encodeUtf8 (T.unwords (map f l))


getConnectionSize :: Environment -> Int
getConnectionSize Development = 1
getConnectionSize Production = 8
getConnectionSize Test = 1

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
  -- FIXME: should come from AEC
  m <- lookupEnv "PORT"
  let p = case m of
        Nothing -> Nothing
        Just s -> Just (read s)
  return p


{-
$envvarConfigSys

A configuration type `AppEnvConfig` is used to represent key/value pairs read from environmental varibles. The purpose is
to allow for the customization of the application to be easy and painless.

This configuration has a whitelist (`appEnvConfigWhitelist`), which are key/value pairs in the config which are okay
to share with the world, primarily to expose to the Mustache system.
-}
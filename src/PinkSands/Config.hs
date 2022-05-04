{-| Application configuration.

This module not only defines settings directly, but also provides a system for defining
configuration settings, like through environmental variables, and more configuration-related
activities.

The Environmental Environment Configuration System
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module PinkSands.Config
    (
    -- * Configuration for Scotty
    -- Configuration specifically for code on the Scotty library level, like the database pool.
      Config(..)
    , ConfigM(..)
    , Environment(..)
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


-- | The mode the application will run under. Depending on such a mode various settings will be used (different database, verbosity, etc.).
data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)


-- | The main configuration records. This is used in a complicated manner with a Scotty monad transformer so the configuration can be read
-- wherever.
--
-- Related: `ConfigM`
data Config = Config
  { environment :: Environment
  , pool :: DB.ConnectionPool
  , appEnvConfig :: AppEnvConfig
  }


-- | See `Config`
newtype ConfigM a = ConfigM
 { runConfigM :: ReaderT Config IO a
 } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)


-- | Environmental variables which allow the app to be easily configured.
--
-- Constructed by `getAppEnvConfig`.
data AppEnvConfig = AppEnvConfig
  { confSiteTitle :: Maybe T.Text 
  -- ^ The site title allows the static site builder to incorporate it as a Mustache variable as
  -- {{confSiteTitle}}.
  } deriving (Show)


-- | Whitelists record lookup functions belonging to `AppEnvConfig`, mapping them to a label which shall be used
-- for Mustache {{variable}} substitutions. Effectively in the form of (key/label, lookup function).
appEnvConfigWhitelist :: [(T.Text, AppEnvConfig -> Maybe T.Text)]
appEnvConfigWhitelist =
    [ ("siteTitle", confSiteTitle)
    ]


-- | This prefix is used before all AppEnvConfig envvars (see `getAppEnvConfig`).
appEnvConfPrefix :: String
appEnvConfPrefix = "SCOTTY_"


-- | Create the raw application configuration records by reading environmental variables.
--
-- An example use is using envvars to set 
getAppEnvConfig :: IO AppEnvConfig
getAppEnvConfig = do
  AppEnvConfig <$>
    lookupEnvPrefixed "SITE_TITLE"
 where
  lookupEnvPrefixed :: String -> IO (Maybe T.Text)
  lookupEnvPrefixed s = do
      maybeEnvValue <- lookupEnv . (appEnvConfPrefix ++) $ s
      pure $ fmap T.pack maybeEnvValue


{-
$envvarConfigSys

A configuration type `AppEnvConfig` is used to represent key/value pairs read from environmental varibles. The purpose is
to allow for the customization of the application to be easy and painless.

This configuration has a whitelist (`appEnvConfigWhitelist`), which are key/value pairs in the config which are okay
to share with the world, primarily to expose to the Mustache system.
-}
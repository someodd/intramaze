{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module PinkSands.Config
    ( AppEnvConfig(..)
    , getAppEnvConfig
    , appEnvConfigWhitelist
    , Environment(..)
    , Config(..)
    , ConfigM(..)
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import qualified Database.Persist.Postgresql as DB


data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)


data Config = Config
  { environment :: Environment
  , pool :: DB.ConnectionPool
  , appEnvConfig :: AppEnvConfig
  }


-- how does this work FIXME
newtype ConfigM a = ConfigM
 { runConfigM :: ReaderT Config IO a
 } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)


-- TODO/FIXME: should the mustache variable names actually be defined herein? the whitelist?
-- | Environmental variables which allow the app to be configured.
data AppEnvConfig = AppEnvConfig
  { confSiteTitle :: Maybe T.Text 
  -- ^ The site title allows the static site builder to incorporate it as a Mustache variable as
  -- {{confSiteTitle}}.
  }


  -- The key in the pair labels a whitelisted (as not to leak anything sensitive!) lookup
  -- to the actual lookup function (belonging to `AppEnvConfig`). The pairs are ( {{mustacheVarname}}, lookupFunc ).
-- | Whitelists various values in the `AppEnvConfig` by mapping them to a label which shall be used
-- for Mustache {{variable}} substitutions.
appEnvConfigWhitelist :: [(T.Text, AppEnvConfig -> Maybe T.Text)]
appEnvConfigWhitelist =
    [ ("siteTitle", confSiteTitle)
    ]


-- | All of the `AppEnvConfig` values are derived from envvars. This is
-- the prefix used in the name of the envvars.
appEnvConfPrefix :: String
appEnvConfPrefix = "SCOTTY_"


getAppEnvConfig :: IO AppEnvConfig
getAppEnvConfig = do
  AppEnvConfig <$>
    lookupEnvPrefixed "SITE_TITLE"
 where
  -- FIXME: can simplify this and do it pointfree
  lookupEnvPrefixed :: String -> IO (Maybe T.Text)
  lookupEnvPrefixed s = do
      maybeEnvValue <- lookupEnv . (appEnvConfPrefix ++) $ s
      pure $ fmap T.pack maybeEnvValue
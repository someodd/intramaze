{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module IntraMaze.Middle where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.Lazy as TL (Text)
import qualified Database.Persist.Postgresql as DB
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Web.Scotty.Trans (ActionT, finish, json)
import qualified Web.Scotty.Trans as Scotty (status)

import IntraMaze.Config


data ApiError = ApiError
  { status :: Int
  -- ^ The HTTP status code representing the error.
  , error :: String
  -- ^ Details about the error.
  }
  deriving (Generic, Show)
instance ToJSON ApiError where


-- | API Error resulting in an error code and a JSON error message being sent and the
-- action being finished.
jsonError :: ApiError -> ActionT Error ConfigM a
jsonError apiError@(ApiError httpCode _) = do
  Scotty.status . toEnum $ httpCode
  json apiError
  finish


-- FIXME: move environment and config to config module!


-- some kind of monad transformer? NOTE figure out
runDB :: (MonadTrans t, MonadIO (t ConfigM)) =>
  DB.SqlPersistT IO a -> t ConfigM a
runDB q = do
  p <- lift (asks pool)
  liftIO (DB.runSqlPool q p)

type Error = TL.Text
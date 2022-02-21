{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module PinkSands.Middle where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.Lazy as TL (Text)
import qualified Database.Persist.Postgresql as DB
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

import PinkSands.Config


data ApiError = ApiError { errorMessage :: String } deriving (Generic, Show)
instance ToJSON ApiError where


-- FIXME: move environment and config to config module!



-- some kind of monad transformer? NOTE figure out
runDB :: (MonadTrans t, MonadIO (t ConfigM)) =>
  DB.SqlPersistT IO a -> t ConfigM a
runDB q = do
  p <- lift (asks pool)
  liftIO (DB.runSqlPool q p)

type Error = TL.Text
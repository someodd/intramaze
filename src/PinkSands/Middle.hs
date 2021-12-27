{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module PinkSands.Middle where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.Lazy as TL (Text)
import qualified Database.Persist.Postgresql as DB
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)


data ApiError = ApiError { errorMessage :: String } deriving (Generic, Show)
instance ToJSON ApiError where
  

data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)


data Config = Config
  { environment :: Environment
  , pool :: DB.ConnectionPool
  }


-- how does this work FIXME
newtype ConfigM a = ConfigM
 { runConfigM :: ReaderT Config IO a
 } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)


-- some kind of monad transformer? NOTE figure out
runDB :: (MonadTrans t, MonadIO (t ConfigM)) =>
  DB.SqlPersistT IO a -> t ConfigM a
runDB q = do
  p <- lift (asks pool)
  liftIO (DB.runSqlPool q p)

type Error = TL.Text
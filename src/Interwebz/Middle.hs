{-
Might be migrating error stuff to its own module.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Interwebz.Middle where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import qualified Database.Persist.Postgresql as DB
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Web.Scotty.Trans (ActionT, finish, json, ScottyError (stringError, showError))
import qualified Web.Scotty.Trans as Scotty (status)
import Database.PostgreSQL.Simple.Errors

import Interwebz.Config
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Database.PostgreSQL.Simple (SqlError)
import qualified Data.UUID as UUID
import Interwebz.Models (RowUUID(RowUUID))
import Slug (Slug, SlugException (..), parseSlug)
import Control.Monad.Catch (try)
import qualified Control.Monad.Reader as Control.Monad.Trans.Reader


-- FIXME: rename to ApiFailure?
data ApiError = ApiError
  { errorStatus :: Int
  -- ^ The HTTP status code representing the error.
  , errorName :: ErrorName
  -- ^ Provided so JavaScript can match against specific errors.
  , errorMessage :: String
  -- ^ Details about the error.
  -- FIXME: maybe friendly message/user message doens't make sense for now?
  --, errorUserMessage :: Maybe String
  -- ^ Friendly non-technical error message to be displayed to a user. Best for specific situations,
  -- where the logic of failure in case user input or otherwise is known and caught for user's sake.
  }
  deriving (Generic, Show)
instance ToJSON ApiError where


instance ApiResponse ApiError where
  getStatus (ApiError a _ _) = a

data ApiSuccess a = ApiSuccess
  { successStatus :: Int
  -- ^ The HTTP success code (there's more than just plain old 200!)
  , successContent :: a
  -- ^ Information from the successful action
  }
  deriving (Generic, Show)
instance ToJSON a => ToJSON (ApiSuccess a) where

instance ToJSON a => ApiResponse (ApiSuccess a) where
  getStatus (ApiSuccess a _) = a

class ToJSON a => ApiResponse a where
  getStatus :: (ToJSON a) => a -> Int

  jsonResponse :: a -> ActionT ApiError ConfigM b
  jsonResponse a = do
    let httpCode = getStatus a
    Scotty.status . toEnum $ httpCode
    json a
    finish


instance ScottyError ApiError where
  stringError s = do
    ApiError 500 UnknownError $ "Special automagically handled error. This means there was some kind of error that popped up that was so unexpected that the code was not written in a way to handle it. Details: " ++ s
  showError e = TL.pack $ show e


-- FIXME: move environment and config to config module!

-- FIXME: use a helper function to get error description?
-- hopefully generally these are reusable and not just for one specific instance. although sometimes that's unavoidable.
-- almost a category of error?
data ErrorName
  = DatabaseUniqueViolation
  | DatabaseNotNullViolation
  | DatabaseForeignKeyViolation
  | DatabaseCheckViolation
  | DatabaseExclusionViolation
  | MissingField
  -- ^ Request was missing a required field.
  | UsernameInvalid
  -- ^ Client tried to submit a username which lacks the characteristics of a proper username.
  | ResourceNotFound
  -- ^ Some resource wasn't found in database.
  | AuthenticationFailure
  -- ^ Authentication failure. Like login info incorrect, or not provided.
  | PermissionFailure
  -- ^ User doesn't have the right permissions to perform action.
  | UuidParseFailure
  -- ^ Attempted to parse a UUID, but failed.
  | UsernameUnavailable
  -- ^ During registration process, the user attempted to create an account with a username which already exists.
  | UnknownError
  -- ^ An error which isnt explicitly caught or known. Generated message.
  deriving (Show, Generic)
instance ToJSON ErrorName where




-- This comes in useful when custom catchers are used with `runDbWitchCatcher` and 
--unknownCatcher :: SqlError -> ConstraintViolation -> IO (Either ApiError a)
--unknownCatcher e t = pure $ Left $ ApiError 500 UnknownError $ show t ++ ": " ++ show e


-- FIXME: remove bad because sometimes intrnal server error, sometimes it's client so... or refactor
toRowUuid :: String -> String -> ActionT ApiError ConfigM RowUUID
toRowUuid s errorString =
    case UUID.fromString s of
      Nothing -> jsonResponse $ ApiError 500 UuidParseFailure errorString
      Just uuid -> pure . RowUUID $ uuid


type Catcher a = SqlError -> ConstraintViolation -> IO (Either ApiError a)


{- | Database error catcher.

Can also be used for overriding and using custom errors. For example:

  customCatcher e (UniqueViolation "unique_username") = pure $ Left $ Middle.ApiError 400 Middle.UsernameTaken $ "The provided username is already taken. More details: " ++ show e
  customCatcher e t = Middle.catcher e t
-}
catcher :: Catcher a
catcher e (UniqueViolation someColumn) =
  pure $ Left $ ApiError
    500
    DatabaseUniqueViolation
    ("Attempted to update the database, but said value already exists on a column which requires unique values (" ++ show someColumn ++ "). Details: " ++ show e)
catcher e (NotNullViolation someColumn) =
  pure $ Left $ ApiError
    500
    DatabaseNotNullViolation
    ("Attempted to set " ++ show someColumn ++ " to null (not allowed)." ++ "Details: " ++ show e)
catcher e (ForeignKeyViolation someTable constraint) =
  pure $ Left $ ApiError
    500
    DatabaseForeignKeyViolation
    ("There was a problem with the constraint " ++ show constraint ++ " on the table " ++ show someTable ++ ". Details: " ++ show e)
-- FIXME: use relationName and contraint rather than holes
catcher e (CheckViolation _ _) =
  pure $ Left $ ApiError
    500
    DatabaseCheckViolation
    (show e)
    -- FIXME: should be better than "nothing!"
-- FIXME: use the field.
catcher e (ExclusionViolation _) =
  pure $ Left $ ApiError
    500
    DatabaseExclusionViolation
    (show e)


{-}
runDbNoScotty
  :: DB.SqlPersistT IO a
  -> ActionT ApiError ConfigM a
-}
runDbNoScotty :: Control.Monad.Trans.Reader.ReaderT DB.SqlBackend IO a -> IO a
runDbNoScotty q = do
  conf <- getConfig
  p <- pure $ pool conf
  --p <- lift (asks pool)
  s <- liftIO $ fmap Right $ DB.runSqlPool q p
  case s of
    Left ae -> pure ae
    Right a -> pure a


runDB
  :: DB.SqlPersistT IO a
  -> ActionT ApiError ConfigM a
runDB q = do
  runDbWithCatcher catcher q


runDbWithCatcher
  :: (SqlError -> ConstraintViolation -> IO (Either ApiError a))
  -> DB.SqlPersistT IO a
  -> ActionT ApiError ConfigM a
runDbWithCatcher catcher' q = do
  p <- lift (asks pool)
  s <- liftIO $ catchViolation catcher' $ fmap Right $ DB.runSqlPool q p
  case s of
    Left ae -> jsonResponse ae
    Right a -> pure a


usernameSlug :: T.Text -> IO (Either SlugException Slug)
usernameSlug username' = do
  (something :: Either SlugException Slug) <- try (parseSlug username' :: IO Slug)
  pure something
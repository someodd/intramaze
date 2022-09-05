{- | Database helper functions and other database related things.

For actual database models look at the `Models` module.
-}
{-# LANGUAGE OverloadedStrings #-}
module Interwebz.Database where

import Control.Monad.IO.Class (liftIO)
import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import Interwebz.Models (Account, RowUUID(..))
import Database.Persist (PersistValue(..), LiteralType (Escaped), keyToValues)
import qualified Data.UUID as UUID
import qualified Data.ByteString.UTF8 as BSU
import Web.Scotty.Trans (ActionT)
import qualified Interwebz.Middle as Middle
import Interwebz.Config (ConfigM(..))
import Data.Text (Text)


-- FIXME: could also use Either!
entityToRowUuid :: DB.PersistEntity a => DB.Entity a -> ActionT Middle.ApiError ConfigM RowUUID
entityToRowUuid (DB.Entity someId _)= do
  matchText . head $ keyToValues someId
 where
  matchText :: PersistValue -> ActionT Middle.ApiError ConfigM RowUUID
  matchText (PersistLiteral_ Escaped t) = do
    case UUID.fromString $ BSU.toString t of
      Nothing -> Middle.jsonResponse $ Middle.ApiError 500 Middle.UuidParseFailure "Database UUID somehow failed to be parsed as a UUID!"
      Just uuid -> pure . RowUUID $ uuid
  matchText e = Middle.jsonResponse $ Middle.ApiError 500 Middle.ResourceNotFound $ " I was looking for a UUID, but found " ++ show e ++ ". This is entirely the fault of the server-end code. This should only happen if the schema changed, like the type of the table key changing or similar."


-- FIXME: redundant because of entityToRowUuid
-- | Helper function to parse/get the `RowUUID` from a account entity.
accountEntityToUuid :: DB.Entity Account -> Either (Middle.ErrorName, String) (RowUUID, Account)
accountEntityToUuid accountEntity = do
    let (DB.Entity accountId account) = accountEntity
    authorId <- case keyToValues accountId of
        [PersistLiteral_ Escaped authorId'] -> Right authorId'
        pv -> Left $ (Middle.UnknownError, show pv  ++ " was not of expected type when looking for author UUID")
    case UUID.fromString $ BSU.toString authorId of
        Nothing -> Left (Middle.UuidParseFailure, "Author UUID somehow failed to be parsed as a UUID!")
        Just uuid -> Right (RowUUID uuid, account)


-- | Scotty failure if cannot parse.
accountEntityToUuid' :: DB.Entity Account -> ActionT Middle.ApiError ConfigM RowUUID
accountEntityToUuid' = either (\(errorName, errorMessage) -> Middle.jsonResponse $ Middle.ApiError 500 errorName errorMessage) (pure . fst) . accountEntityToUuid


{- | Create a user account.

Handles actions for failure purposes.
-}
createAccount :: Middle.Catcher () -> Text -> Text -> ActionT Middle.ApiError ConfigM ()
createAccount catcher username password = do
    _ <- usernameValidator username
    -- FIXME: this can raise an error SqlError and it won't be caught/transformed into a scotty error!
    Middle.runDbWithCatcher catcher (DB.rawExecute "INSERT INTO \"account\" (username, password) VALUES (?, crypt(?, gen_salt('bf')))" [DB.PersistText username, DB.PersistText password])
 where
  usernameValidator username' = do
    eitherExceptionOrSlug <- liftIO $ Middle.usernameSlug username'
    case eitherExceptionOrSlug of
      Left slugException -> Middle.jsonResponse $ Middle.ApiError 400 Middle.UsernameInvalid $ "The username you're trying to create is invalid: " ++ show slugException
      Right sl -> pure sl


defaultAdminPassword :: Text
defaultAdminPassword = "password"

defaultAdminUsername :: Text
defaultAdminUsername = "admin"


-- | Create the default admin account.
createDefaultAdmin :: ActionT Middle.ApiError ConfigM ()
createDefaultAdmin = do
    createAccount Middle.catcher defaultAdminUsername defaultAdminPassword
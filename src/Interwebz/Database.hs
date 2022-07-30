{- | Database helper functions and other database related things.

For actual database models look at the `Models` module.
-}
{-# LANGUAGE OverloadedStrings #-}
module Interwebz.Database where

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


--createAccount :: Text -> Text -> 
createAccount :: Middle.Catcher () -> Text -> Text -> ActionT Middle.ApiError ConfigM ()
createAccount catcher username password = do
    -- FIXME: this can raise an error SqlError and it won't be caught/transformed into a scotty error!
    Middle.runDbWithCatcher catcher (DB.rawExecute "INSERT INTO \"account\" (username, password) VALUES (?, crypt(?, gen_salt('bf')))" [DB.PersistText username, DB.PersistText password])


defaultAdminPassword :: Text
defaultAdminPassword = "password"

defaultAdminUsername :: Text
defaultAdminUsername = "admin"


-- | Create the default admin account.
createDefaultAdmin :: ActionT Middle.ApiError ConfigM ()
createDefaultAdmin = do
    createAccount Middle.catcher defaultAdminUsername defaultAdminPassword
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IntraMaze.Models where

import GHC.Generics ( Generic )
import Data.Text (Text)
import qualified Data.Text as T ( pack, unpack )
import qualified Data.Text.Lazy as TL ( unpack )
--import Data.Text.Encoding as TSE ( decodeUtf8 )
import Data.ByteString.UTF8 as BSU ( toString, fromString )
--import Data.Time.Clock (UTCTime)
--import Data.Proxy (Proxy(..))
import Database.Persist.Types (LiteralType(..))
import Database.Persist.Sql (PersistFieldSql(..), SqlType(..))
import Database.Persist.PersistValue (PersistValue(..))
import Database.Persist.Class.PersistField (PersistField(..))
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share,
  sqlSettings)
import Web.HttpApiData (ToHttpApiData(..), FromHttpApiData(..))
--import Data.Aeson.Parser (jsonAccum)
import qualified Data.UUID as UUID (UUID, fromString, toString)
import Data.Aeson.TH (defaultOptions)
import Data.Aeson (genericToEncoding, FromJSON(..), ToJSON(..))
import Web.Scotty (Parsable)
import Web.Scotty.Trans (Parsable(parseParam))
--import GHC.Base (Eq)
import Web.PathPieces (PathPiece(..))
import Database.Persist (fieldHaskell)
import Database.Persist (getEntityFields)
import qualified Database.Persist as Database.Persist.Names
import qualified Database.Persist as Database.Persist.Types.Base
--import qualified Data.ByteString as BS

-- FIXME: should use actual
-- polygon in database instead. how does JSON statement here work?
-- postgres handles uuid and polygons but that doesn't work if just
-- doing a json statement type thing
-- should be double floats
newtype Polygon = Polygon [(Int, Int)] deriving (Generic, Show)

-- these are working via some kind of generic magic... should learn about that
instance ToJSON Polygon where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Polygon where
  -- nothing!

-- TODO: needs to have a Polygon type too
instance PersistFieldSql Polygon where
  -- PostgreSQL supports "uuid" as a column type.
  sqlType _ = SqlOther "polygon"


swapFirstAndLastChars :: Char -> Char -> String -> String
swapFirstAndLastChars firstChar lastChar string =
  [firstChar] ++ (init . drop 1 $ string) ++ [lastChar]


instance PersistField Polygon where
  toPersistValue (Polygon coords) =
    -- NOTE: do we want to use DbSpecifc here instead
    -- because of JSON? "The DbSpecific constructor
    -- corresponds to the legacy PersistDbSpecific
    -- constructor. We need to keep this around because
    -- old databases may have serialized JSON 
    -- representations that reference this. We don't want
    -- to break the ability of a database to load rows."
    PersistLiteral_ Escaped . BSU.fromString $ swapFirstAndLastChars '(' ')' (show coords)
  fromPersistValue (PersistLiteral_ _ byteString) =
    let
      (asCoords :: [(Int, Int)]) = read $ swapFirstAndLastChars '[' ']' (BSU.toString byteString)
    in
      Right . Polygon $ asCoords
  fromPersistValue persistValue = Left . T.pack $ "Invalid Polygon:" ++ show persistValue


instance Show RowUUID where
  show (RowUUID a) = show a


newtype RowUUID = RowUUID UUID.UUID deriving (Generic, Eq, Ord, Read, ToHttpApiData, FromHttpApiData)
instance PathPiece RowUUID where
  fromPathPiece text = RowUUID <$> UUID.fromString (T.unpack text)
  toPathPiece (RowUUID uuid) = T.pack $ UUID.toString uuid

-- | This makes it so we can use `param` and parse it immediately as a RoomID.
instance Parsable RowUUID where
  -- text -> either text a
  parseParam text =
    case UUID.fromString (TL.unpack text) of
      Just uuid -> Right . RowUUID $ uuid
      Nothing -> Left $ "Invalid UUID: " <> text

-- these are working via some kind of generic magic... should learn about that
instance ToJSON RowUUID where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON RowUUID where
  -- nothing!

-- TODO: needs to have a Polygon type too
instance PersistFieldSql RowUUID where
  -- PostgreSQL supports "uuid" as a column type.
  sqlType _ = SqlOther "uuid"


instance PersistField RowUUID where
  toPersistValue (RowUUID uuid) =
    -- NOTE: do we want to use DbSpecifc here instead
    -- because of JSON? "The DbSpecific constructor
    -- corresponds to the legacy PersistDbSpecific
    -- constructor. We need to keep this around because
    -- old databases may have serialized JSON 
    -- representations that reference this. We don't want
    -- to break the ability of a database to load rows."
    PersistLiteral_ Escaped . BSU.fromString $ UUID.toString uuid
  fromPersistValue (PersistLiteral_ _ byteString) =
    case UUID.fromString (BSU.toString byteString) of
      Just uuid -> Right . RowUUID $ uuid
      Nothing -> Left . T.pack $ "Invalid UUID: " <> BSU.toString byteString
  fromPersistValue persistValue = Left . T.pack $ "Invalid UUID:" ++ show persistValue


getEntityFieldsHaskell
  :: Database.Persist.Types.Base.EntityDef
  -> [Database.Persist.Names.FieldNameHS]
getEntityFieldsHaskell someEntity = do
  map fieldHaskell $ getEntityFields someEntity


{-
User json
  username Text
  password BS.ByteString
  salt BS.ByteString
  UniqueUsername username
-}


-- would it be better if portals was field of rooms?
--instance ToMustache Room

-- change roomuuid to uuid! FIXME (account should not be roomuuid but instead just Uuid! or DbUuid or something)

-- Portal needs to make belongsTo+coordinates unique?
-- this should all link together using roomID not RoomId. we want
-- to link using UUIDs not autoincrement ids.
-- should i use actual integer ids or room uuids
share [mkMigrate "migrateAll", mkPersist sqlSettings] [persistLowerCase|
Account json
  Id RowUUID default=gen_random_uuid()
  username Text
  password Text
  root Bool default=False
  UniqueUsername username

Portal json
  belongsTo RoomId
  linksTo RoomId
  coordinates Polygon
  deriving Show

Room json
  Id RowUUID default=gen_random_uuid()
  title Text Maybe
  description Text Maybe
  bgFileName Text Maybe
  author AccountId
  deriving Show
|]

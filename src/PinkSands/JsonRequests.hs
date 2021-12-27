-- | Models for JSON requests. Defines the structure of the request and validation tools.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
module PinkSands.JsonRequests where

import Control.Monad (MonadPlus (mzero))
import qualified Data.Aeson as Aeson (FromJSON(..), Value (..))
import Data.Aeson ((.:))
import GHC.Generics ( Generic )
import qualified Data.Text.Encoding as TSE
import qualified Data.ByteString as ByteString (ByteString)
import Web.Scotty.Trans (ActionT, jsonData, status)

import qualified PinkSands.JWT as JWT (UserClaims(..), decodeAndValidateFull)
import qualified PinkSands.Models as Models (Room(..), Key(..), RoomUUID(..))
import PinkSands.Middle (ConfigM, Error, ApiError(..))
import Control.Monad.IO.Class (liftIO)
import Web.Scotty.Trans (json, finish)
import Network.HTTP.Types.Status (notFound404)
import PinkSands.Models (EntityField(..), Room, Portal)
import qualified Database.Persist as DB
import Database.Persist (Update)
import Data.Maybe (catMaybes)
import qualified Data.Text as T


-- | My hacky way of having something like `Room`, without the requirement of
-- specifying the room's author during a request (that's already in the token).
--
-- Maybe this is a bad idea and I should force the user to specify the author
-- always so you can also change the author?
data UnownedRoom = UnownedRoom
    { unownedRoomTitle :: Maybe T.Text
    , unownedRoomDescription :: Maybe T.Text 
    , unownedRoomBgFileName :: Maybe T.Text
    } deriving (Generic, Show, Aeson.FromJSON)


-- | Many room requests (JSON) use this format.
data GenericRoomRequestUnvalidated = GenericRoomRequestUnvalidated
    { genericRoomRequestUnvalidatedRoom :: UnownedRoom
    , genericRoomRequestUnvalidatedToken :: Token
    } deriving (Generic, Show)


instance Aeson.FromJSON GenericRoomRequestUnvalidated where
    parseJSON (Aeson.Object v) = GenericRoomRequestUnvalidated
        <$> (v .: "room")
        <*> (v .: "token")
    parseJSON _ = mzero


data GenericRoomRequestValidated = GenericRoomRequestValidated
    { genericRoomRequestValidatedRoom :: Models.Room
    , genericRoomRequestValidatedUserClaims :: JWT.UserClaims
    }


-- should i make an instance for Token->userclaims? maybe that'd be handy
class Aeson.FromJSON a => ValidatedRequest a b | b -> a where
    -- | Turn an unvalidated request into a validated one (or error)
    validateRequest :: a -> ActionT Error ConfigM (Either String b)

    -- FIXME: needs a better name.
    -- | Fail with the passed error on invalid JSON (namely due to token failure, generally,
    -- but can be other errors).
    apiErrorLeft :: ActionT Error ConfigM b
    apiErrorLeft = do
        t <- jsonData
        maybeValidJson <- validateRequest t
        failLeft maybeValidJson


-- | Similar to `UnownedRoom`, this makes it so you do not have to specify
-- a field that is already indicated somewhere else. Namely, the room it
-- is a part of...
--data UnlinkedPortal = UnlinkedPortal
data CreatePortalUnvalidated = CreatePortalUnvalidated
    { createPortalUnvalidatedPortal :: Portal 
    , createPortalUnvalidatedToken :: Token 
    } deriving (Generic, Show)


data RoomUpdateValidated = RoomUpdateValidated JWT.UserClaims [Update Room] deriving (Generic)


instance ValidatedRequest GenericRoomRequestUnvalidated RoomUpdateValidated where
    validateRequest roomUnvalidated = do
        userClaims <- getUserClaims $ genericRoomRequestUnvalidatedToken roomUnvalidated
        -- needs to validate token too... needs to be same as author or root
        let
            room = genericRoomRequestUnvalidatedRoom roomUnvalidated
            -- FIXME: I feel like this should be possible with some more abstraction so
            -- I don't need to be explicit here...
            (transformedList :: [Maybe (Update Room)]) =
                [ unownedRoomTitle room >>= Just . (RoomTitle DB.=.) . Just
                , unownedRoomDescription room >>= Just . (RoomDescription DB.=.) . Just
                , unownedRoomBgFileName room >>= Just . (RoomBgFileName  DB.=.) . Just
                ]
        pure . Right $ RoomUpdateValidated userClaims $ catMaybes transformedList


getUserClaims' :: Token -> ActionT Error ConfigM (Either String JWT.UserClaims)
getUserClaims' (Token token) = do
    unvalidatedToken <- liftIO $ JWT.decodeAndValidateFull $ pure token
    case unvalidatedToken of
      Left errorString -> do
          pure . Left $ errorString
      Right uc -> pure . Right $ uc


-- FIXME: i'd like to use Status as argument here. but maybe Left should be
-- (String, Status)? And ApiError should also take the status into account?
failLeft :: Either String a -> ActionT Error ConfigM a
failLeft something = do
    case something of
        Left errorString -> do
            -- FIXME: bad error code!
            status notFound404 
            json . ApiError $ errorString
            finish
        Right a ->
            pure a


-- FIXME: this could be abstracted to Either so both the instance and
-- this could use it... use either boilerplate
-- FIXME: does this perhaps belong in JWT instead?
getUserClaims :: Token -> ActionT Error ConfigM JWT.UserClaims
getUserClaims token = getUserClaims' token >>= failLeft


-- | This is useful for actions which only need a token/to be authorized in 
-- some way.
instance ValidatedRequest Token JWT.UserClaims where
    validateRequest = getUserClaims'


instance ValidatedRequest GenericRoomRequestUnvalidated GenericRoomRequestValidated where
    validateRequest roomUnvalidated = do
        userClaims <- getUserClaims $ genericRoomRequestUnvalidatedToken roomUnvalidated
        let room = genericRoomRequestUnvalidatedRoom roomUnvalidated
        pure . Right $ GenericRoomRequestValidated
                { genericRoomRequestValidatedRoom = Models.Room
                    (unownedRoomTitle room)
                    (unownedRoomDescription room)
                    (unownedRoomBgFileName room)
                    -- FIXME: RoomUUID can be renamed simply to ObjectUUID or something!
                    (Models.AccountKey . Models.RoomUUID $ JWT.userId userClaims)
                , genericRoomRequestValidatedUserClaims = userClaims
                }


newtype Token = Token ByteString.ByteString deriving (Generic, Show)


instance Aeson.FromJSON Token where
    parseJSON (Aeson.String v) = Token . TSE.encodeUtf8 <$> pure v
    parseJSON _ = mzero
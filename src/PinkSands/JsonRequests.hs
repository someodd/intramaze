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
import GHC.Generics ( Generic )
import qualified Data.Text.Encoding as TSE
import qualified Data.ByteString as ByteString (ByteString)
import Web.Scotty.Trans (ActionT, jsonData, status, json, finish)

import qualified PinkSands.JWT as JWT (UserClaims(..), decodeAndValidateFull)
import qualified PinkSands.Models as Models (Room(..), EntityField(..))
import PinkSands.Middle (Error, ApiError(..))
import PinkSands.Config (ConfigM)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status (notFound404)
import qualified Database.Persist as DB
import Database.Persist (Update)
import Data.Maybe (catMaybes)


-- | Many room requests (JSON) use this format.
data GenericRoomRequestUnvalidated = GenericRoomRequestUnvalidated
    { genericRoomRequestUnvalidatedRoom :: Models.Room
    , genericRoomRequestUnvalidatedToken :: Token
    } deriving (Generic, Show, Aeson.FromJSON)


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


data RoomUpdateValidated = RoomUpdateValidated JWT.UserClaims [Update Models.Room] deriving (Generic)


instance ValidatedRequest GenericRoomRequestUnvalidated RoomUpdateValidated where
    validateRequest roomUnvalidated = do
        userClaims <- getUserClaims $ genericRoomRequestUnvalidatedToken roomUnvalidated
        -- needs to validate token too... needs to be same as author or root
        let
            room = genericRoomRequestUnvalidatedRoom roomUnvalidated
            -- FIXME: I feel like this should be possible with some more abstraction so
            -- I don't need to be explicit here... some kind of trick... maybe having to
            -- do with Persistent directly... could be pairs
            -- should be able to use a command to get both sides?
            pairs =
                [ (Models.roomTitle, Models.RoomTitle)
                , (Models.roomDescription, Models.RoomDescription)
                , (Models.roomBgFileName, Models.RoomBgFileName)
                ]
            (transformedList :: [Maybe (Update Models.Room)]) = map
                (\(x,y) -> x room >>= Just . (y DB.=.) . Just)
                pairs
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


-- | For requests which only use a token object and nothing else.
data TokenRequestUnvalidated = TokenRequestUnvalidated { tokenA :: Token } deriving (Generic, Aeson.FromJSON)


-- | This is useful for actions which only need a token/to be authorized in 
-- some way.
instance ValidatedRequest TokenRequestUnvalidated JWT.UserClaims where
    validateRequest (TokenRequestUnvalidated token) = getUserClaims' token


instance ValidatedRequest GenericRoomRequestUnvalidated GenericRoomRequestValidated where
    validateRequest roomUnvalidated = do
        userClaims <- getUserClaims $ genericRoomRequestUnvalidatedToken roomUnvalidated
        let room = genericRoomRequestUnvalidatedRoom roomUnvalidated
        pure . Right $ GenericRoomRequestValidated
                { genericRoomRequestValidatedRoom = room
                , genericRoomRequestValidatedUserClaims = userClaims
                }


-- FIXME: doesn't this already exist in JWT? why do it twice?
newtype Token = Token ByteString.ByteString deriving (Generic, Show)

instance Aeson.FromJSON Token where
    parseJSON (Aeson.String v) = Token . TSE.encodeUtf8 <$> pure v
    parseJSON _ = mzero
-- FIXME: maybe not just for JSON requests since validates headers too! maybe just call it
-- "request validation."
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
module IntraMaze.JsonRequests where

import Control.Monad (MonadPlus (mzero))
import GHC.Generics ( Generic )
import qualified Data.Text.Encoding as TSE
import qualified Data.ByteString as ByteString (ByteString)
import Web.Scotty.Trans (ActionT, jsonData, header)
import qualified Data.Text.Lazy.Encoding as TLE ( encodeUtf8 )

import qualified IntraMaze.JWT as JWT (UserClaims(..), decodeAndValidateFull)
import qualified IntraMaze.Models as Models (Room(..), EntityField(..))
import qualified IntraMaze.Middle as Middle (Error, ApiError(..), jsonError)
import IntraMaze.Config (ConfigM)
import Control.Monad.IO.Class (liftIO)
import qualified Database.Persist as DB
import Database.Persist (Update)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as BL
import IntraMaze.JWT (UserClaims)
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:))


-- | Many room requests (JSON) use this format.
data GenericRoomRequestUnvalidated = GenericRoomRequestUnvalidated
    { genericRoomRequestUnvalidatedRoom :: Models.Room
    } deriving (Generic, Show)
    --     } deriving (Generic, Show, Aeson.FromJSON)


instance Aeson.FromJSON GenericRoomRequestUnvalidated where
    parseJSON (Aeson.Object v) =
        GenericRoomRequestUnvalidated
            <$> v .: "description"
    parseJSON invalid = Aeson.typeMismatch "GenericRoomRequestUnvalidated" invalid


data GenericRoomRequestValidated = GenericRoomRequestValidated
    { genericRoomRequestValidatedRoom :: Models.Room
    , genericRoomRequestValidatedUserClaims :: JWT.UserClaims
    }




class Aeson.FromJSON a => ValidatedRequest a b | b -> a where
    -- NOTE: I could have just done `Either String b`, to make it so the status code can be decided later.
    -- | Turn an unvalidated request into a validated one (or error)
    validateRequest :: a -> ActionT Middle.Error ConfigM (Either Middle.ApiError b)

    -- FIXME: needs a better name.
    -- | Fail with the passed error on invalid JSON (namely due to token failure, generally,
    -- but can be other errors).
    apiErrorLeft :: ActionT Middle.Error ConfigM b
    apiErrorLeft = do
        -- FIXME: this will error if no JSON (why should it if the model is blank!? some requests don't require json)
        t <- jsonData
        maybeValidJson <- validateRequest t
        failLeft maybeValidJson


data RoomUpdateValidated = RoomUpdateValidated JWT.UserClaims [Update Models.Room] deriving (Generic)


instance ValidatedRequest GenericRoomRequestUnvalidated RoomUpdateValidated where
    validateRequest roomUnvalidated = do
        userClaims <- getUserClaimsOrFail
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


-- FIXME: four functions that could be maybe combined?


getUnvalidatedToken :: ActionT Middle.Error ConfigM (Either Middle.ApiError Token)
getUnvalidatedToken = do
    token <- header "Authorization"
    case token of
      Nothing -> pure . Left $ Middle.ApiError 401 "Missing authorization header."
      Just txt -> pure $ Right . Token . BL.toStrict . TLE.encodeUtf8 $ txt


-- | Read the JWT from the request, ensure that it's valid or produce an HTTP `Error`,
-- otherwise return the `UserClaims` corresponding to the token found in the request.
getUserClaimsOrFail :: ActionT Middle.Error ConfigM UserClaims
getUserClaimsOrFail = getUnvalidatedToken >>= failLeft >>= getUserClaims


getUserClaims' :: Token -> ActionT Middle.Error ConfigM (Either Middle.ApiError JWT.UserClaims)
getUserClaims' (Token token) = do
    unvalidatedToken <- liftIO $ JWT.decodeAndValidateFull $ pure token
    case unvalidatedToken of
      Left errorString -> do
          pure . Left $ Middle.ApiError 401 errorString
      Right uc -> pure . Right $ uc


-- FIXME: this could be abstracted to Either so both the instance and
-- this could use it... use either boilerplate
-- FIXME: does this perhaps belong in JWT instead?
getUserClaims :: Token -> ActionT Middle.Error ConfigM JWT.UserClaims
getUserClaims token = getUserClaims' token >>= failLeft


-- FIXME: i'd like to use Status as argument here. but maybe Left should be
-- (String, Status)? And ApiError should also take the status into account?
failLeft :: Either Middle.ApiError a -> ActionT Middle.Error ConfigM a
failLeft = either Middle.jsonError pure


instance ValidatedRequest GenericRoomRequestUnvalidated GenericRoomRequestValidated where
    validateRequest roomUnvalidated = do
        userClaims <- getUserClaimsOrFail
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
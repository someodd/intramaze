-- FIXME: maybe not just for JSON requests since validates headers too! maybe just call it
-- "request validation."
-- | Models for JSON requests. Defines the structure of the request and validation tools.
-- FIXME: i guess this shouldn't be handling permissions, right? or maybe it should because validation... keep things separate?
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

{- | JSON request verification.

Parses an expected permissive JSON data structure into a datatype which has
fully validated the information to be safe (such as for inserting in a
database). This includes authorization/permission checks.

Verification only performs checks based off the request data provided. No checks
are ran against the database.

-}
module Interwebz.JsonRequests where

import Control.Monad (MonadPlus (mzero))
import GHC.Generics ( Generic )
import qualified Data.Text.Encoding as TSE
import qualified Data.ByteString as ByteString (ByteString)
import Web.Scotty.Trans (ActionT, jsonData, header)
import qualified Data.Text.Lazy.Encoding as TLE ( encodeUtf8 )

import qualified Interwebz.JWT as JWT (UserClaims(..), decodeAndValidateFull)
import qualified Interwebz.Models as Models (Room(..), EntityField(..))
import qualified Interwebz.Middle as Middle (ApiError(..), jsonResponse, ErrorName (AuthenticationFailure))
import Interwebz.Config (ConfigM)
import Control.Monad.IO.Class (liftIO)
import qualified Database.Persist as DB
import Database.Persist (Update)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as BL
import Interwebz.JWT (UserClaims (..))
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import Interwebz.Models (Room(..), RowUUID (..), Key (..))

{- | A sloppy hack for making a version of Models.Room not require Authors so

I hope to replace this witha  more elegant solution in the future.

This is the request format that gets transformed into the validated "good data."

Unvalidated room request that doesn't require anything defined.
-}
data RoomRequest = RoomRequest
    { title :: Maybe Text
    , description :: Maybe Text
    , bgFileName :: Maybe Text
    } deriving (Generic, Show, Aeson.FromJSON)

-- | Mapping to easily transform `RoomRequest` into `Models.Room`.
roomRequestMapping
    :: [(RoomRequest -> Maybe Text, DB.EntityField Models.Room (Maybe Text))]
roomRequestMapping =
    [ (title, Models.RoomTitle)
    , (description, Models.RoomDescription)
    , (bgFileName, Models.RoomBgFileName)
    ]

{- | Built list of update operations for a `Models.Room`, validated and based
off a request.

-}
data ValidatedRoomUpdate = ValidatedRoomUpdate JWT.UserClaims [Update Models.Room] deriving (Generic)

{- | Build a series of update operations.

Always requires JWT.

Does not validate that the user defined in JWT has permissions to modify the
room.
-}
instance ValidatedRequest RoomRequest ValidatedRoomUpdate where
    validateRequest roomUnvalidated = do
        userClaims <- getUserClaimsOrFail
        let
            -- FIXME: I feel like this should be possible with some more abstraction so
            -- I don't need to be explicit here... some kind of trick... maybe having to
            -- do with Persistent directly... could be pairs
            -- should be able to use a command to get both sides?
            (transformedList :: [Maybe (Update Models.Room)]) = map
                (\(x,y) -> x roomUnvalidated >>= Just . (y DB.=.) . Just)
                roomRequestMapping
        pure . Right $ ValidatedRoomUpdate userClaims $ catMaybes transformedList

-- | A full `Models.Room` which has been validated from request.
data ValidatedRoom = ValidatedRoom Models.Room UserClaims

{- | Validate a request that uses a full `Models.Room` in the request.

Ensures the JWT user matches the defined room author in the request (or root).
-}
instance ValidatedRequest Models.Room ValidatedRoom where
    validateRequest roomUnvalidated = do
        userClaims <- getUserClaimsOrFail
        if (AccountKey . RowUUID $ userId userClaims) == roomAuthor roomUnvalidated || isRoot userClaims
            then pure . Right $ ValidatedRoom roomUnvalidated userClaims
            else pure . Left $ Middle.ApiError 403 Middle.AuthenticationFailure $ "Unless you are root, you may not set the author field to another user. User info from JWT was " ++ show userClaims ++ ", but request specified " ++ show (roomAuthor roomUnvalidated)


class Aeson.FromJSON a => ValidatedRequest a b | b -> a where
    -- FIXME: is this concept bad because it should all be done by validateRequest?
    -- NOTE: I could have just done `Either String b`, to make it so the status code can be decided later.
    -- | Turn an unvalidated request into a validated one (or error)
    validateRequest :: a -> ActionT Middle.ApiError ConfigM (Either Middle.ApiError b)

    -- FIXME: needs a better name.
    -- | Fail with the passed error on invalid JSON (namely due to token failure, generally,
    -- but can be other errors).
    apiErrorLeft :: ActionT Middle.ApiError ConfigM b
    apiErrorLeft = do
        -- FIXME: this will error if no JSON (why should it if the model is blank!? some requests don't require json)
        t <- jsonData
        maybeValidJson <- validateRequest t
        failLeft maybeValidJson


-- FIXME: four functions that could be maybe combined?


getUnvalidatedToken :: ActionT Middle.ApiError ConfigM (Either Middle.ApiError Token)
getUnvalidatedToken = do
    -- FIXME/TODO: needs to be "Authoirzation: Bearer <token>" -- check documentation on this...
    token <- header "Authorization"
    case token of
      Nothing -> pure . Left $ Middle.ApiError 401 Middle.AuthenticationFailure "Missing authorization header."
      Just txt -> pure $ Right . Token . BL.toStrict . TLE.encodeUtf8 $ txt


-- | Read the JWT from the request, ensure that it's valid or produce an HTTP `Error`,
-- otherwise return the `UserClaims` corresponding to the token found in the request.
getUserClaimsOrFail :: ActionT Middle.ApiError ConfigM UserClaims
getUserClaimsOrFail = getUnvalidatedToken >>= failLeft >>= getUserClaims


getUserClaims' :: Token -> ActionT Middle.ApiError ConfigM (Either Middle.ApiError JWT.UserClaims)
getUserClaims' (Token token) = do
    unvalidatedToken <- liftIO $ JWT.decodeAndValidateFull $ pure token
    case unvalidatedToken of
      Left errorString -> do
          pure . Left $ Middle.ApiError 401 Middle.AuthenticationFailure errorString
      Right uc -> pure . Right $ uc


-- FIXME: this could be abstracted to Either so both the instance and
-- this could use it... use either boilerplate
-- FIXME: does this perhaps belong in JWT instead?
getUserClaims :: Token -> ActionT Middle.ApiError ConfigM JWT.UserClaims
getUserClaims token = getUserClaims' token >>= failLeft


-- FIXME: i'd like to use Status as argument here. but maybe Left should be
-- (String, Status)? And ApiError should also take the status into account?
failLeft :: Either Middle.ApiError a -> ActionT Middle.ApiError ConfigM a
failLeft = either Middle.jsonResponse pure



-- FIXME: doesn't this already exist in JWT? why do it twice?
newtype Token = Token ByteString.ByteString deriving (Generic, Show)

instance Aeson.FromJSON Token where
    parseJSON (Aeson.String v) = Token . TSE.encodeUtf8 <$> pure v
    parseJSON _ = mzero


-- | Require some boolean property of the UserClaims.
jwtRequire :: Token  -> (UserClaims -> Bool) -> String -> ActionT Middle.ApiError ConfigM (Either String UserClaims)
jwtRequire token jwtSucceedCondition failString = do
  userClaims <- getUserClaims token
  if jwtSucceedCondition userClaims
    then pure $ Right userClaims
    else pure $ Left failString
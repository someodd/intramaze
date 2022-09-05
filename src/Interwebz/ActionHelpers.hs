{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | API for managing the website as well as helpers for Actions.

Could be called API, but not sure. The idea of this module is to declutter
Actions.hs.

This helps keep Actions.hs nice and to the point with which endpoints it is
offering.

Might get merged into Middle.
-}
module Interwebz.ActionHelpers (
  createUserProfile,
  generateRoom,
  toKey,
  UsernamePassword (..),
  mustBeRoomAuthorOrRoot',
  mustBeRoomAuthorOrRoot,
  Action,
  needRoot,
  buildEverything,
  mustMatchUuidOrRoot,
  notFoundA,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), Value (Null))
import Data.Text.Lazy (Text)
import Database.Persist (Entity (..))
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (notFound404, status403, status404)
import Web.Scotty.Trans (ActionT, finish, json, param, status)

import Interwebz.Config (ConfigM (..))
import Interwebz.JWT (UserClaims (..))
import qualified Interwebz.JsonRequests as JsonRequests
import qualified Interwebz.Middle as Middle
import Interwebz.Models
import Interwebz.Static (buildProfile, buildProfilePages, createNewRoom, getUserRooms, setupEssentials)

-- | The default kind of action we use throughout the source.
type Action = ActionT Middle.ApiError ConfigM ()

{- | Just for ease-of-use for jsonData.

This makes handling certain actions which use a username and password easier.
-}
data UsernamePassword = UsernamePassword
  { username :: Text
  , password :: Text
  }
  deriving (Generic, Show)

instance FromJSON UsernamePassword

-- | Abstraction for creating a profile for a specific user.
createUserProfile ::
  -- | UUID of the specific user account to generate a profile for.
  RowUUID ->
  ActionT Middle.ApiError ConfigM FilePath
createUserProfile rowUuid = do
  rooms <- getUserRooms rowUuid
  maybeAccount <- Middle.runDB (DB.get (AccountKey rowUuid))
  case maybeAccount of
    Nothing ->
      Middle.jsonResponse $
        Middle.ApiError 404 Middle.ResourceNotFound $
          "Attempted to update the profile belonging to user of id " ++ show rowUuid ++ ", but no such user ID exists in database."
    Just account -> do
      liftIO $ buildProfile (account, rooms)

{- | Static generation of a room based off the RowUUID.

The return value is the file path to the directory belonging to the generated
room.

API 404 error if specified UUID doesn't exist.

Helper function.

May be refactored to be partially moved to Static or moved to Static entirely.
-}
generateRoom ::
  -- | UUID of the room.
  RowUUID ->
  ActionT Middle.ApiError ConfigM FilePath
generateRoom uuid = do
  roomMaybe <- Middle.runDB $ DB.get (RoomKey uuid)
  portals <- Middle.runDB $ DB.selectList [PortalBelongsTo DB.==. RoomKey uuid] []
  case roomMaybe >>= \room -> Just $ createNewRoom uuid room [entityVal portal | portal <- portals] of
    Nothing -> do
      notFoundA
      finish
    Just roomPath -> liftIO roomPath

{- | Create everything for the static site.

Returns all the paths built.

May be moved to the `Interwebz.Static` module soon.
-}
buildEverything :: ActionT Middle.ApiError ConfigM [FilePath]
buildEverything = do
  (roomKeySelection :: [DB.Key Room]) <- Middle.runDB (DB.selectKeysList [] [])
  profilePaths <- buildProfilePages
  paths <- traverse (\(RoomKey uuid) -> generateRoom uuid) roomKeySelection
  builtStaticPaths <- liftIO setupEssentials
  pure $ paths ++ builtStaticPaths ++ profilePaths

{- | From row ID (`Integer`) to another Persistent key type.

May be moved to another module in the future.

>>> import Interwebz.Models (PortalId)
>>> toKey 1 :: PortalId
PortalKey {unPortalKey = SqlBackendKey {unSqlBackendKey = 1}}
-}
toKey :: DB.ToBackendKey DB.SqlBackend a => Integer -> DB.Key a
toKey i = DB.toSqlKey (fromIntegral (i :: Integer))

{- | Ensures that the specified UUID matches the JWT, or that the JWT
authenticates the user as having root privileges.

The request's JWT must belong to the user as specified by the `RowUUID`, or the
request will fail with an API error.
-}
mustMatchUuidOrRoot :: RowUUID -> ActionT Middle.ApiError ConfigM ()
mustMatchUuidOrRoot rowUuid = do
  userClaims <- JsonRequests.getUserClaimsOrFail
  if RowUUID (userId userClaims) == rowUuid || isRoot userClaims
    then pure ()
    else do
      _ <- error "wtf"
      status status403
      json $ Middle.ApiError 403 Middle.PermissionFailure "Must be the user of the profile attempting to update or root to perform this action."
      finish

{- | Handles getting a room  (from request) if we are the author (or root)
according to the generic room request, or an error is provided.

Errors on failed JWT permission, otherwise returns the requested room UUID and
the room itself.
-}
mustBeRoomAuthorOrRoot :: RowUUID -> UserClaims -> ActionT Middle.ApiError ConfigM Room
mustBeRoomAuthorOrRoot rowUuid userClaims = do
  m <- Middle.runDB $ DB.get (RoomKey rowUuid)
  case m of
    Nothing -> do
      status status404
      finish
    Just room -> do
      -- FIXME: need to rename roomuuid to MyUUID
      if roomAuthor room == (AccountKey . RowUUID $ userId userClaims) || isRoot userClaims
        then pure room
        else do
          status status403
          json $ Middle.ApiError 403 Middle.PermissionFailure "Must be author or root to perform this action."
          finish

{- | Same as `mustBeRoomAuthorOrRoot`, but the `JWT` and room UUID is taken from
the request. The Room object is submitted in request. Only for JSON requests.
-}
mustBeRoomAuthorOrRoot' :: ActionT Middle.ApiError ConfigM (RowUUID, Room)
mustBeRoomAuthorOrRoot' = do
  i <- param "id"
  (JsonRequests.ValidatedRoom room userClaims) <- JsonRequests.apiErrorLeft
  _ <- mustBeRoomAuthorOrRoot i userClaims
  pure (i, room)

-- | Perform an `Action` if the `UserClaims` have the `root` property.
needRoot :: UserClaims -> String -> Action -> Action
needRoot userClaims errorMessage action = do
  if isRoot userClaims
    then action
    else Middle.jsonResponse $ Middle.ApiError 403 Middle.PermissionFailure errorMessage

-- | Generic 404.
notFoundA :: Action
notFoundA = do
  status notFound404
  json Null

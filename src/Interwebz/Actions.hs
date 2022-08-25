{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Actions for the `Web.Scotty` REST API app.

These functions (`Web.Scotty.Trans.ActionT` [actions]) are mapped to various
routes in another module. Note how they start with an HTTP verb and end in
'A'.

-}
module Interwebz.Actions (
  getWhoamiA,
  postUserA,
  getUserTokenA,
  getRoomsA,
  postRoomsA,
  getRoomGenerateA,
  getGenerateEverythingA,
  putRoomA,
  getGenerateProfilesA,
  getGenerateSpecificProfileA,
  postRoomsImageA,
  getRoomA,
  getRoomSearchA,
  deleteRoomA,
  getRoomsPortalsA,
  deletePortalsA,
  postPortalA,
  getPortalsA,
  patchRoomA,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (Null))
import Data.ByteString.UTF8 as BS (toString)
import qualified Data.Text as T
import Data.Text.Encoding as TSE (decodeUtf8)
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy as TL
import qualified Data.UUID as UUID
import Database.Persist (Entity, Filter (Filter), FilterValue (..), getBy)
import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import Database.PostgreSQL.Simple.Errors (ConstraintViolation (UniqueViolation))
import Interwebz.ActionHelpers
import Interwebz.Config (ConfigM)
import Interwebz.Database (accountEntityToUuid', createAccount)
import Interwebz.JWT (UserClaims (..))
import qualified Interwebz.JWT as JWT (makeToken)
import qualified Interwebz.JsonRequests as JsonRequests
import qualified Interwebz.Middle as Middle
import Interwebz.Models (Account (..), EntityField (..), Key (..), Portal (portalBelongsTo), PortalId, Room (..), RowUUID (..), Unique (..))
import Interwebz.Static (buildProfilePages, createNewRoom, createRoomImage)
import Network.HTTP.Types.Status (created201, status204)
import Network.Wai.Parse (FileInfo (..))
import Web.Scotty.Trans (
  ActionT,
  files,
  finish,
  json,
  jsonData,
  param,
  status,
 )

{- | Action which only returns the `UserClaims` found in the request's JWT.

Can fail if there isn't a JWT, if the JWT is invalid, etc.
-}
getWhoamiA :: Action
getWhoamiA = do
  (uc :: UserClaims) <- JsonRequests.getUserClaimsOrFail
  json uc

{- | Create (register) a new user and create their profile page.

If there isn't a problem, a JSON response is generated including the
user's UUID and the file path to their newly created profile page.

Various errors can occur when attempting to register a user, such as:

  * Username taken
  * Username or password missing
-}
postUserA :: ActionT Middle.ApiError ConfigM ()
postUserA = do
  (t :: UsernamePassword) <- jsonData
  _ <- createAccount catcher (TL.toStrict $ username t) (TL.toStrict $ password t)

  -- Now we need to query the database just to get the new user's uuid.
  maybeAccount :: Maybe (Entity Account) <- Middle.runDB $ getBy . UniqueUsername $ toStrict $ username t
  createdUserRowUUID <-
    maybe
      (Middle.jsonResponse $ Middle.ApiError 500 Middle.ResourceNotFound "Successfully created the user account, yet cannot find it by the username provided. The profile page cannot be created and there will likely be other errors associated with this account.")
      accountEntityToUuid'
      maybeAccount

  -- Now attempt to create a profile page for the user created in the database
  filePath <- createUserProfile createdUserRowUUID

  Middle.jsonResponse $ Middle.ApiSuccess 201 (createdUserRowUUID, filePath)
 where
  -- In case the user attempts to create an account with a username which already exists.
  catcher e (UniqueViolation "unique_username") =
    pure $ Left $ Middle.ApiError 400 Middle.UsernameUnavailable $ "The provided username is already taken. More details: " ++ show e
  catcher e t = Middle.catcher e t

-- | Return a JWT from the provided username and password, or return an error.
getUserTokenA :: Action
getUserTokenA = do
  (t :: UsernamePassword) <- jsonData
  (accountEntities :: [DB.Entity Account]) <- Middle.runDB (DB.rawSql "SELECT ?? FROM \"account\" WHERE username = ? AND password = crypt(?, password);" [DB.PersistText . TL.toStrict $ username t, DB.PersistText . TL.toStrict $ password t])
  case accountEntities of
    (DB.Entity accountId account) : _ -> do
      case UUID.fromString . show $ unAccountKey accountId of
        (Just userUuid :: Maybe UUID.UUID) -> do
          token <- liftIO $ JWT.makeToken userUuid (TL.toStrict $ username t) (accountRoot account)
          Middle.jsonResponse $ Middle.ApiSuccess 200 (TSE.decodeUtf8 token)
        Nothing ->
          Middle.jsonResponse $ Middle.ApiError 500 Middle.UuidParseFailure "failed to parse UUID from DB to a UUID type during JWT process"
    [] -> do
      Middle.jsonResponse $ Middle.ApiError 403 Middle.AuthenticationFailure "Credentials provided are incorrect."

{- | Get a list of all of the rooms.

This honestly is a waste of resources and will require admin or be deleted in
the future.
-}
getRoomsA :: Action
getRoomsA = do
  ts <- Middle.runDB (DB.selectList [] [])
  json (ts :: [DB.Entity Room])

{- | REST endpoint for creating a room (which doesn't already exist).

Simply requires any authenticated user, which then becomes the author of that
room.

Creates the static files and directory for the room.

Returns the created path to the room (static HTML directory) and the UUID of the
newly created room, otherwise an error message (authentication failure).
-}
postRoomsA :: Action
postRoomsA = do
  (createRoomValidated :: JsonRequests.GenericRoomRequestValidated) <- JsonRequests.apiErrorLeft
  let room = JsonRequests.genericRoomRequestValidatedRoom createRoomValidated
  (RoomKey uuid) <- Middle.runDB (DB.insert room)
  filePath <- liftIO $ createNewRoom uuid room []
  status created201
  json (filePath, uuid)

{- | REST endpoint for calling for the static HTML file of a room.

Requires a JWT/authenticated user (or there will be a failiure).

Can fail in the event of the requested room `RowUUID` not being present in the
database.

May change to POST HTTP verb in the future since it's creating HTTP files? Or
maybe another action, I just don't feel like GET is right.
-}
getRoomGenerateA :: Action
getRoomGenerateA = do
  (i :: RowUUID) <- param "id"
  (_ :: UserClaims) <- JsonRequests.getUserClaimsOrFail
  roomPath <- generateRoom i
  status created201
  json roomPath

{- | REST endpoint for creating the static HTML files for ALL the
rooms/portals/entire site, as well as essential files.

Will be accessible by a CLI in the future. Also, may change HTTP verb in the
future to POST or something other than GET.
-}
getGenerateEverythingA :: Action
getGenerateEverythingA = do
  (userClaims :: UserClaims) <- JsonRequests.getUserClaimsOrFail
  needRoot userClaims "You need to be root to generate all the static files." $ do
    builtFiles <- buildEverything
    status created201
    json builtFiles

{- | Generate all the static profile pages for each user.

API error if lack root permission.

May change the HTTP verb in the future.
-}
getGenerateProfilesA :: Action
getGenerateProfilesA = do
  (userClaims :: UserClaims) <- JsonRequests.getUserClaimsOrFail
  needRoot userClaims "You need to be root to create all the profile page static files." $ do
    pathsOfProfilePages <- buildProfilePages
    status created201
    json pathsOfProfilePages

{- | Re/generate an account page for a specific user.

API error if lack root permission, or if not authenticated as the user whose
profile is requested for update.

May change the HTTP verb in the future.
-}
getGenerateSpecificProfileA :: Action
getGenerateSpecificProfileA = do
  (i :: RowUUID) <- param "id"
  _ <- mustMatchUuidOrRoot i
  filePath <- createUserProfile i
  json filePath

{- | Endpoint for uploading the room image (for the specified room).

Expects `multipart/form-data` as encoding type.

API error if not room author or root.

Returns the path to the uploaded image.
-}
postRoomsImageA :: Action
postRoomsImageA = do
  (i, _) <- mustBeRoomAuthorOrRoot'
  files' <- files
  let fileInfo = case files' of
        ("image", fileInfo') : _ -> fileInfo'
        _ -> error "nothing"
      imageFileName = fileName fileInfo
  Middle.runDB (DB.update (RoomKey i) [RoomBgFileName DB.=. Just (decodeUtf8 imageFileName)])
  pathToImage <- liftIO $ createRoomImage i (fileContent fileInfo) (toString imageFileName)
  status created201
  json pathToImage

{- | Handles a request to get (some of) a specific room's data from the database.

Returns 404 if the room is not found.
-}
getRoomA :: Action
getRoomA = do
  i <- param "id"
  m <- Middle.runDB (DB.get (RoomKey i))
  case m of
    Nothing -> notFoundA
    Just t -> json (t :: Room)

{- | Search/query rooms by description partial match/full text search.

Returns 404 if no rooms have a match, otherwise returns rooms that have the
substring.
-}
getRoomSearchA :: Action
getRoomSearchA = do
  (description :: T.Text) <- param "description"
  m <- Middle.runDB $ DB.selectList [Filter RoomDescription (FilterValue . Just $ "%" <> description <> "%") (DB.BackendSpecificFilter "ILIKE")] []
  case m of
    [] -> notFoundA
    (x :: [DB.Entity Room]) -> json x

{- | Handle request to update existing room.

Performs a `DB.repsert`.

Returns an API error if invalid room UUID in request, or if the user does not
have permission to change the room.

Return value may change in near future to have a better API success message.
-}
putRoomA :: Action
putRoomA = do
  (i, roomFromRequest) <- mustBeRoomAuthorOrRoot'
  Middle.runDB (DB.repsert (RoomKey i) roomFromRequest)
  json roomFromRequest

{- | Delete a room by the specified room UUID and all of its portals.

Returns an API error if invalid room UUID or permission error.
-}
deleteRoomA :: Action
deleteRoomA = do
  (roomUUID, _) <- mustBeRoomAuthorOrRoot'
  Middle.runDB (DB.delete (RoomKey roomUUID))
  Middle.runDB (DB.deleteWhere [PortalBelongsTo DB.==. RoomKey roomUUID])
  json Null

{- | Handle a request to get a list of all portals belonging to a room specified
by UUID.

Currently no real API error handling, this will change in near future.
-}
getRoomsPortalsA :: Action
getRoomsPortalsA = do
  (i :: RowUUID) <- param "id"
  m <- Middle.runDB (DB.selectList [PortalBelongsTo DB.==. RoomKey i] [])
  json (m :: [DB.Entity Portal])

{- | Handle request to delete a portal by ID.

API error if the requestor is not authenticated as root or the author of the room
which the portal belongs to.
-}
deletePortalsA :: Action
deletePortalsA = do
  (i :: Integer) <- param "id" -- ID of portal to delete
  -- first we must check the author of the portal in order to perform a permission check
  m <- Middle.runDB (DB.get (toKey i))
  portal <- case m of
    Nothing -> do
      notFoundA
      finish
    Just (t :: Portal) -> pure t
  let correspondingRoomId = portalBelongsTo portal
  correspondingRoomUUID <-
    case UUID.fromString . show $ unRoomKey correspondingRoomId of
      (Just roomUuid :: Maybe UUID.UUID) -> pure $ RowUUID roomUuid
      Nothing ->
        Middle.jsonResponse $ Middle.ApiError 500 Middle.UuidParseFailure "failed to parse UUID from DB to a UUID type during portal author/permission check process"
  userClaims <- JsonRequests.getUserClaimsOrFail
  _ <- mustBeRoomAuthorOrRoot correspondingRoomUUID userClaims
  -- now the portal can be deleted
  Middle.runDB (DB.delete (toKey i :: PortalId))
  status status204
  json Null

{- | Handle request to create a portal.

May change return data to the inserted portal's row ID in the future.
-}
postPortalA :: Action
postPortalA = do
  t <- jsonData
  Middle.runDB (DB.insert_ t)
  status created201
  json (t :: Portal)

{- | Handle a request for all portals in the database.

This is a bad idea for resources and there's no use I can think of, so it may be
behind root required or simply deleted in the future.
-}
getPortalsA :: Action
getPortalsA = do
  ts <- Middle.runDB (DB.selectList [] [])
  json (ts :: [DB.Entity Portal])

{- | Handle request to update certain room fields.

Currently only supports updating the title and description.

Will support every field except the image(s) in the future.
-}
patchRoomA :: Action
patchRoomA = do
  -- make sure the user has permissions to change the room
  i <- param "id"
  (JsonRequests.RoomUpdateValidated userClaims roomUpdates) <- JsonRequests.apiErrorLeft
  _ <- mustBeRoomAuthorOrRoot i userClaims

  -- now we can start manipulation
  Middle.runDB (DB.update (RoomKey i) $ roomUpdates)
  json Null
-- TODO: all responses should include a response code, error type info, and then the actual response.
-- | Actions (routes) for handling requests...
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module IntraMaze.Actions where

import Data.Aeson ( FromJSON(..), Value(Null) )
import GHC.Generics ( Generic )
import Data.ByteString.UTF8 as BS (toString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans)
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import IntraMaze.Models (AccountId, Room (..), Key(..), Portal, RowUUID (..), EntityField(..), PortalId, Account (..))
import qualified Data.UUID as UUID
import Network.HTTP.Types.Status (created201,
    notFound404, status204, status404, status403)
import Network.Wai.Parse (FileInfo(..))
import Web.Scotty.Trans (ActionT,
    json, jsonData, param, status, files, finish)
import IntraMaze.Static (createRoomImage, createNewRoom, setupEssentials, buildProfilePages, getUserRooms, buildProfile)
import Database.Persist (Entity (entityVal), Filter (Filter), FilterValue (..))
import qualified IntraMaze.Middle as Middle
import qualified Data.Text.Lazy as TL
import qualified IntraMaze.JWT as JWT (makeToken, UserClaims (userId))
import Data.Text.Encoding as TSE ( decodeUtf8 )
import IntraMaze.JWT (UserClaims (..))
import qualified IntraMaze.JsonRequests as JsonRequests
import IntraMaze.Config ( ConfigM )


type Action = ActionT Middle.Error ConfigM ()


-- belongs in json requests FIXME?
-- | Just for ease-of-use for jsonData.
data UsernamePassword = UsernamePassword
  { username :: Text
  , password :: Text
  } deriving (Generic, Show)
instance FromJSON UsernamePassword where


-- | Gets the user's UUID.
getWhoamiA :: Action
getWhoamiA = do
    (uc :: UserClaims) <- JsonRequests.getUserClaimsOrFail
    json uc


-- | Create a new user.
postUserA :: Action
postUserA = do
  (t :: UsernamePassword) <- jsonData -- need to get username and password from this
  _ <- Middle.runDB (DB.rawExecute "INSERT INTO \"account\" (username, password) VALUES (?, crypt(?, gen_salt('bf')))" [DB.PersistText . TL.toStrict $ username t, DB.PersistText . TL.toStrict $ password t])
  status created201
  -- FIXME: should give back json of the uuid?
  json (1 :: Int)


-- FIXME: more elegant status on fail. use ApiError or that higher level one...
-- | Log in, creating/return a JWT, or an error.
postUserLoginA :: Action
postUserLoginA = do
  (t :: UsernamePassword) <- jsonData -- need to get username and password from this
  (accountEntities :: [DB.Entity Account]) <- Middle.runDB (DB.rawSql "SELECT ?? FROM \"account\" WHERE username = ? AND password = crypt(?, password);" [DB.PersistText . TL.toStrict $ username t, DB.PersistText . TL.toStrict $ password t])
  case accountEntities of
    (DB.Entity accountId account):_ -> do
      status created201
      --json $ makeToken $ (username t) (show . head $ userIds)
      case UUID.fromString . show $ unAccountKey accountId of
        (Just userUuid :: Maybe UUID.UUID) -> do
          token <- liftIO $ JWT.makeToken userUuid (TL.toStrict $ username t) (accountRoot account) -- FIXME XXX
          json $ TSE.decodeUtf8 token
        Nothing -> json ("failed to parse UUID from DB to a UUID type during JWT process" :: Text)
    [] -> do
      status created201
      json ("failed" :: Text)


-- FIXME: didn't you make a function that can accept a boolean function for inspecting user claims?
postTestRequire :: Action
postTestRequire = do
  (t :: JsonRequests.Token) <- jsonData
  userClaims <- jwtRequire t (\uc -> JWT.userId uc == (let (Just uuid) = UUID.fromString "4efeb8a5-d6a9-4f4a-8f0b-7950fbce9636" in uuid)) "not the user I'm looking for!"
  json $ "it worked! " ++ show userClaims


getRoomsA :: Action
getRoomsA = do
  ts <- Middle.runDB (DB.selectList [] [])
  json (ts :: [DB.Entity Room])


-- | REST endpoint for creating a room (which doesn't already exist).
--
-- Simply requires any authenticated user, which then becomes the author
-- of that room.
--
-- Creates the static files and directory for the room.
postRoomsA :: Action
postRoomsA = do
  (createRoomValidated :: JsonRequests.GenericRoomRequestValidated) <- JsonRequests.apiErrorLeft
  let room = JsonRequests.genericRoomRequestValidatedRoom createRoomValidated
  -- could use insert instead of insert_ to get the key back and we can give the key as a response or add it to
  -- jsonData?
  (RoomKey uuid) <- Middle.runDB (DB.insert room)
  -- FIXME: should we be pointing the database to this? it'd make sense.
  filePath <- liftIO $ createNewRoom uuid room []
  status created201
  json (filePath, uuid)


-- FIXME: where to put this instead?
-- FIXME: messy because you're not used to dealing with transformers and monads this much
-- | Static generation of a room based off the RowUUID.
--
-- The return value is "Maybe FilePath" instead of simply "FilePath," because
-- no room by the supplied `uuid` may exist!
--
-- Helper function.
generateRoom
  :: (MonadTrans t, MonadIO (t ConfigM))
  => RowUUID
  -> t ConfigM (Maybe FilePath)
generateRoom uuid = do
  roomMaybe <- Middle.runDB $ DB.get (RoomKey uuid)
  portals <- Middle.runDB $ DB.selectList [PortalBelongsTo DB.==. RoomKey uuid] []
  case roomMaybe >>= \room -> Just $ createNewRoom uuid room [entityVal portal | portal <- portals] of
    Nothing -> pure Nothing
    Just roomPath -> do
      path <- liftIO roomPath
      pure $ Just path


-- FIXME: only needs user to regenerate
-- FIXME: POST instead?
-- | REST endpoint for calling for the static HTML file of a room.
getRoomGenerateA :: Action
getRoomGenerateA = do
  (i :: RowUUID) <- param "id"
  roomPathMaybe <- generateRoom i
  case roomPathMaybe of
    Nothing ->
      notFoundA
    Just path -> do
      status created201
      json path


-- FIXME: should produce error if no room author
getRoomAuthor :: RowUUID -> ActionT Middle.Error ConfigM (Either Middle.ApiError AccountId)
getRoomAuthor rowUuid = do
  m <- Middle.runDB (DB.get (RoomKey rowUuid))
  case m of
    Nothing -> pure . Left $ Middle.ApiError 404 "not found"
    Just t -> pure $ Right $ roomAuthor (t :: Room)


-- | Error out if no room...
getRoomAuthor' :: RowUUID -> ActionT Middle.Error ConfigM AccountId
getRoomAuthor' rowUuid = getRoomAuthor rowUuid >>= JsonRequests.failLeft


-- FIXME: should have separate thing for just regenerating rooms nad not entire site?
-- TODO: generate just the essentials as its own command...
-- FIXME: POST instead? which HTTP verb?
-- | REST endpoint for creating the static HTML files for ALL the rooms/portals/entire site.
--
-- Also setup essential files.
getGenerateEverythingA :: Action
getGenerateEverythingA = do
    (roomKeySelection :: [DB.Key Room]) <- Middle.runDB (DB.selectKeysList [] [])
    -- couldn't this be more elegant? the userclaims stuff? could even make a subclass
    -- which requires using a validatable request type
    (userClaims :: UserClaims) <- JsonRequests.getUserClaimsOrFail
    needRoot userClaims "You need to be root to generate all the static files." $ do
      -- FIXME/TODO: make a static generateEverything helper function
      profilePaths <- buildProfilePages
      paths <- traverse (\(RoomKey uuid) -> generateRoom uuid) roomKeySelection
      builtStaticPaths <- liftIO setupEssentials
      status created201
      json $ [p | Just p <- paths] ++ builtStaticPaths ++ profilePaths


-- | Perform an `Action` if the `UserClaims` have the `root` property.
needRoot :: UserClaims -> String -> Action -> Action
needRoot userClaims errorMessage action = do
  if isRoot userClaims
    then action
    else Middle.jsonError $ Middle.ApiError 403 errorMessage


-- | Generate all the static profile pages for each user.
getGenerateProfiles :: Action
getGenerateProfiles = do
  (userClaims :: UserClaims) <- JsonRequests.getUserClaimsOrFail
  needRoot userClaims "You need to be root to create all the profile page static files." $ do
      pathsOfProfilePages <- buildProfilePages
      status created201
      json pathsOfProfilePages


-- FIXME: root or corresponding user (user corresponds to ID)
-- | Re/generate an account page for a specific user.
getGenerateSpecificProfile :: Action
getGenerateSpecificProfile = do
  (i :: RowUUID) <- param "id"
  _ <- mustMatchUuidOrRoot i
  rooms <- getUserRooms i
  maybeAccount <- Middle.runDB (DB.get (AccountKey i))
  case maybeAccount of
    Nothing ->
      Middle.jsonError $ Middle.ApiError 404 $ "Attempted to update the profile belonging to user of id " ++ show i ++ ", but no such user ID exists in database."
    Just account -> do
      filePath <- liftIO $ buildProfile (account, rooms)
      json [filePath]


-- FIXME: needs to use directory for room ID...
-- FIXME: can I accept token in json body AND all the other stuff?!
-- | Endpoint for uploading the room image (for the specified room).
--
-- Expects `multipart/form-data` as encoding type.
postRoomsImageA :: Action
postRoomsImageA = do
  -- we need to be the room author to do this. get room author and then check that
  -- author in token matches with jwtUserClaim check wahtever FIXME
  (i :: RowUUID) <- param "id"
  (userClaims :: UserClaims) <- JsonRequests.getUserClaimsOrFail
  _ <- mustBeRoomAuthorOrRoot i userClaims
  files' <- files
  let fileInfo = case files' of
                   ("image", fileInfo'):_ -> fileInfo'
                   _ -> error "nothing"
      imageFileName = fileName fileInfo
  Middle.runDB (DB.update (RoomKey i) [RoomBgFileName DB.=. Just (decodeUtf8 imageFileName)])
  pathToImage <- liftIO $ createRoomImage i (fileContent fileInfo) (toString imageFileName)
  status created201
  json pathToImage


getRoomA :: Action
getRoomA = do
  -- can fix this by importing and creating an instance of import Web.Scotty.Action (Parsable) for RowUUID
  i <- param "id"
  --m <- runDB (DB.getBy (UniqueRoomID i))
  --m <- runDB (DB.get (toKey i))
  m <- Middle.runDB (DB.get (RoomKey i))
  case m of
    Nothing -> notFoundA
    Just t -> json (t :: Room)


-- FIXME: also by title?
-- | Search/query rooms by description partial match.
getRoomSearchA :: Action
getRoomSearchA = do
  (description :: T.Text) <- param "description"
  --m <- runDB (DB.selectList [(RoomDescription) DB.==. (RoomKey i)] [])
  -- FIXME: this string substitution is VERY BAD. i also thing there's  abetter way to do this in 0.6.
  m <- Middle.runDB $ DB.selectList [Filter RoomDescription (FilterValue . Just $ "%" <> description <> "%") (DB.BackendSpecificFilter "ILIKE")] []
  json (m :: [DB.Entity Room])


-- FIXME: should these permission type things get moved to JWT or permissions or something?
-- FIXME: very similar to other must be author or root
-- | Ensures that the specified UUID matches the JWT, or that the JWT authenticates
-- the user as having root privileges.
mustMatchUuidOrRoot :: RowUUID -> ActionT Middle.Error ConfigM ()
mustMatchUuidOrRoot rowUuid = do
    userClaims <- JsonRequests.getUserClaimsOrFail
    if RowUUID (userId userClaims) == rowUuid || isRoot userClaims
      then pure ()
      else do
        status status403
        json $ Middle.ApiError 403 "Must be the user of the profile attempting to update or root to perform this action."
        finish


-- FIXME: why duplicate?
mustBeRoomAuthorOrRoot :: RowUUID -> UserClaims -> ActionT Middle.Error ConfigM Room
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
                  json $ Middle.ApiError 403 "Must be author or root to perform this action."
                  finish


-- FIXME: belongs in JsonRequests
-- | Handles getting a room  (from request) if we are the author (or root) according to the generic
-- room request, or an error is provided.
mustBeRoomAuthorOrRoot' :: ActionT Middle.Error ConfigM (RowUUID, Room)
mustBeRoomAuthorOrRoot' = do
    i <- param "id"
    (createRoomValidated :: JsonRequests.GenericRoomRequestValidated) <- JsonRequests.apiErrorLeft
    let userClaims = JsonRequests.genericRoomRequestValidatedUserClaims createRoomValidated
        room = JsonRequests.genericRoomRequestValidatedRoom createRoomValidated
    _ <- mustBeRoomAuthorOrRoot i userClaims
    pure (i, room)


-- TODO: patchRoomA; i guess this is overwriting whereas patch doesn't overwrite empty or
-- something
-- FIXME why does it expect the primary key (custom) in the json even tho it's defined in id?
putRoomA :: Action
putRoomA = do
    -- FIXME: can't use roomupdate for this just use normal one generic
    -- make sure the user has permissions to change the room
    (i, roomFromRequest) <- mustBeRoomAuthorOrRoot'

    -- We now know we are either root or the room's author. proceed to modify.
    Middle.runDB (DB.repsert (RoomKey i) roomFromRequest)
    json roomFromRequest -- would it be better to have success message?


-- | Delete a room by the specified room UUID and all of its portals.
deleteRoomA :: Action
deleteRoomA = do
  (i :: RowUUID) <- param "id"
  (userClaims :: UserClaims) <- JsonRequests.getUserClaimsOrFail
  _ <- mustBeRoomAuthorOrRoot i userClaims

  -- we have the right permissions to delete the room, so we can now delete it.
  Middle.runDB (DB.delete (RoomKey i))
  Middle.runDB (DB.deleteWhere [PortalBelongsTo DB.==. RoomKey i])
  json Null


getRoomsPortalsA :: Action
getRoomsPortalsA = do
  -- can fix this by importing and creating an instance of import Web.Scotty.Action (Parsable) for RowUUID
  (i :: RowUUID) <- param "id"
  --m <- runDB (DB.getBy (UniqueRoomID i))
  --m <- runDB (DB.get (toKey i))
  m <- Middle.runDB (DB.selectList [PortalBelongsTo DB.==. RoomKey i] [])
  json (m :: [DB.Entity Portal])


-- FIXME: need room permissions to delete!
deletePortalsA :: Action
deletePortalsA = do
  i <- param "id"
  Middle.runDB (DB.delete (toKey i :: PortalId))
  status status204
  json Null


-- NOTE: won't this have to deal with multiple portals? how will we handle adding mroe than one... patch? i guess you can keep posting them?
postPortalsA :: Action
postPortalsA = do
  t <- jsonData
  -- could use insert instead of insert_ to get the key back and we can give the key as a response or add it to
  -- jsonData?
  Middle.runDB (DB.insert_ t)
  status created201
  json (t :: Portal)


notFoundA :: Action
notFoundA = do
  status notFound404
  json Null


getPortalsA :: Action
getPortalsA = do
  ts <- Middle.runDB (DB.selectList [] [])
  json (ts :: [DB.Entity Portal])


-- | update room records. currently only supports updating the title and description.
-- FIXME: needs to be fixed because it should be able to update many fields at once! this is terrible.
patchRoomA :: Action
patchRoomA = do
    -- make sure the user has permissions to change the room
    i <- param "id"
    (JsonRequests.RoomUpdateValidated userClaims roomUpdates) <- JsonRequests.apiErrorLeft
    _ <- mustBeRoomAuthorOrRoot i userClaims

    -- now we can start manipulation
    Middle.runDB (DB.update (RoomKey i) $ roomUpdates)
    json Null


-- FIXME: does this belong in jsonrequests or jwt?
-- | Require some boolean property of the UserClaims.
jwtRequire :: JsonRequests.Token  -> (UserClaims -> Bool) -> String -> ActionT Middle.Error ConfigM (Either String UserClaims)
jwtRequire token jwtSucceedCondition failString = do
  userClaims <- JsonRequests.getUserClaims token
  if jwtSucceedCondition userClaims
    then pure $ Right userClaims
    else pure $ Left failString


-- FIXME: belongs somewhere else
-- NOTE: I don't understand the fromIntegral/Integer bit of this function. 
-- Doesn't it already know it's an integer, since that's in the function's signature?
toKey :: DB.ToBackendKey DB.SqlBackend a => Integer -> DB.Key a
toKey i = DB.toSqlKey (fromIntegral (i :: Integer))
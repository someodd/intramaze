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
module Interwebz.ActionHelpers
  ( createUserProfile
  , generateRoom
  , toKey
  ) where

import Control.Monad.IO.Class (liftIO)
import Database.Persist (Entity (..))
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import Interwebz.Config (ConfigM (..))
import qualified Interwebz.Middle as Middle
import Interwebz.Models (EntityField (..), Key (..), RowUUID)
import Interwebz.Static (buildProfile, createNewRoom, getUserRooms)
import Web.Scotty.Trans (ActionT)


-- | Abstraction for creating a profile for a specific user.
createUserProfile
  :: RowUUID
  -- ^ UUID of the specific user account to generate a profile for.
  -> ActionT Middle.ApiError ConfigM FilePath
createUserProfile rowUuid = do
  rooms <- getUserRooms rowUuid
  maybeAccount <- Middle.runDB (DB.get (AccountKey rowUuid))
  case maybeAccount of
    Nothing ->
      Middle.jsonResponse
        $ Middle.ApiError 404 Middle.ResourceNotFound
        $ "Attempted to update the profile belonging to user of id " ++ show rowUuid ++ ", but no such user ID exists in database."
    Just account -> do
      liftIO $ buildProfile (account, rooms)


{- | Static generation of a room based off the RowUUID.

The return value is "Maybe FilePath" instead of simply "FilePath," because no
room by the supplied `uuid` may exist!

Helper function.
-}
generateRoom
  :: RowUUID
  -- ^ UUID of the room.
  -> ActionT Middle.ApiError ConfigM (Maybe FilePath)
generateRoom uuid = do
  roomMaybe <- Middle.runDB $ DB.get (RoomKey uuid)
  portals <- Middle.runDB $ DB.selectList [PortalBelongsTo DB.==. RoomKey uuid] []
  case roomMaybe >>= \room -> Just $ createNewRoom uuid room [entityVal portal | portal <- portals] of
    Nothing -> pure Nothing
    Just roomPath -> do
      path <- liftIO roomPath
      pure $ Just path


{- | From row ID (`Integer`) to another Persistent key type.

May be moved to another module in the future.

>>> import Interwebz.Models (PortalId)
>>> toKey 1 :: PortalId
PortalKey {unPortalKey = SqlBackendKey {unSqlBackendKey = 1}}
-}
toKey :: DB.ToBackendKey DB.SqlBackend a => Integer -> DB.Key a
toKey i = DB.toSqlKey (fromIntegral (i :: Integer))

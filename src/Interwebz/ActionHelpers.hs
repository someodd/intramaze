{- | API for managing the website as well as helpers for Actions.

Could be called API, but not sure. The idea of this module is to declutter Actions.hs.

This helps keep Actions.hs nice and to the point with which endpoints it is offering.

Might get merged into Middle.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Interwebz.ActionHelpers
  ( createUserProfile
  , generateRoom
  , toKey
  ) where

import Interwebz.Models (RowUUID, Key (..), EntityField (..))
import Web.Scotty.Trans (ActionT)
import qualified Interwebz.Middle as Middle
import Control.Monad.IO.Class (liftIO)
import Interwebz.Config (ConfigM(..))
import qualified Database.Persist.Class as DB
import Interwebz.Static (getUserRooms, buildProfile, createNewRoom)
import qualified Database.Persist as DB
import Database.Persist (Entity(..))
import qualified Database.Persist.Sql as DB


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
      Middle.jsonResponse $ Middle.ApiError 404 Middle.ResourceNotFound $ "Attempted to update the profile belonging to user of id " ++ show rowUuid ++ ", but no such user ID exists in database."
    Just account -> do
      liftIO $ buildProfile (account, rooms)


-- | Static generation of a room based off the RowUUID.
--
-- The return value is "Maybe FilePath" instead of simply "FilePath," because
-- no room by the supplied `uuid` may exist!
--
-- Helper function.
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


-- FIXME: belongs somewhere else
-- NOTE: I don't understand the fromIntegral/Integer bit of this function. 
-- Doesn't it already know it's an integer, since that's in the function's signature?
toKey :: DB.ToBackendKey DB.SqlBackend a => Integer -> DB.Key a
toKey i = DB.toSqlKey (fromIntegral (i :: Integer))
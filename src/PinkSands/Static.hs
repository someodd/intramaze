-- | Builds the static site from the database and other static site generator related things.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module PinkSands.Static (createRoomImage, createNewRoom, setupEssentials, buildProfilePages) where

import qualified Data.Vector as Vector (fromList)
import Data.List (intercalate)
import Data.HashMap.Strict ( fromList )
import Data.Text (unpack, pack)
import qualified Text.Mustache.Types as M (Value(..))
import PinkSands.Models (RoomUUID (..), Room(..), Portal(..), Key (RoomKey, AccountKey), Polygon (Polygon), accountUsername, Account, EntityField (..))
import qualified Data.ByteString.Lazy as BSL (writeFile, ByteString)
import System.FilePath (joinPath, (</>), takeDirectory)
import System.Directory (createDirectoryIfMissing, copyFile, doesDirectoryExist)
import Template
import System.Directory.Recursive (getFilesRecursive)
import Text.Mustache (ToMustache (toMustache))
import qualified Data.HashMap.Strict as HashMap
--import Control.Monad (filterM)
import qualified Data.Text as T
import qualified PinkSands.Config as Conf (getAppEnvConfig, AppEnvConfig(..), appEnvConfigWhitelist)
import qualified Text.Mustache.Types as MTypes
import qualified PinkSands.Middle as Middle
import qualified Database.Persist.Sql as DB
import Web.Scotty.Trans (ActionT)
import PinkSands.Config (ConfigM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.UUID as UUID
import Database.Persist (keyToValues, LiteralType (Escaped))
import Database.Persist.PersistValue (PersistValue(..))
import Data.Maybe (fromJust)
import qualified Data.ByteString.UTF8 as BSU


-- | Where images are stored/uploaded to. This is a hack for now.
--
-- In the future there will be a separate upstream for the images directory,
-- separate from the normal static directory.
roomImagesPath :: FilePath
roomImagesPath = copyPath </> "rooms"


-- | Where files are written out to.
buildPath :: FilePath
buildPath = "built"


-- | Static files for building out HTML files to the buildPath.
staticPath :: FilePath
staticPath = "static"


-- | Path where files get copied from to built, preserving directory structure.
copyPath :: FilePath
copyPath = staticPath </> "copy"


-- | Mustache templates are kept here. Base templates and includes and the like, not main content.
mustacheTemplatesPath :: FilePath
mustacheTemplatesPath = staticPath </> "mustache"


-- | Files in this directory get ran through the custom Mustache renderer and
-- written to the `buildPath`.
mustacheBuildThesePath :: FilePath
mustacheBuildThesePath = staticPath </> "mustache-build"


-- | Mustache search space. Mustache will look for includes and templates in general here.
--
-- Something to beware of: I think the search path order means that whichever path has the
-- file name first will result in not checking any of the other paths.
searchSpace :: [FilePath]
searchSpace = [mustacheTemplatesPath, mustacheBuildThesePath]


-- | Object for Mustache templates.
data RoomObject = RoomObject
  { roomObjId :: RoomUUID
  , roomObjPortals :: [Portal]
  , roomObjTitle :: Maybe T.Text
  , roomObjDescription :: Maybe T.Text
  , roomObjImagePath :: Maybe T.Text
  }


instance ToMustache RoomObject where
  toMustache room =
    M.Object . HashMap.fromList $
      [ ("imagePath" :: T.Text, maybe M.Null M.String (roomObjImagePath room))
      , ("description", maybe M.Null M.String (roomObjDescription room))
      -- FIXME/TODO: should it perhaps be M.Null instead of bool?
      , ("title", maybe M.Null M.String (roomObjTitle room))
      , ("areas", M.Array area)
      , ("id", M.String . pack . show $ roomObjId room)
      ]
   where
    area = Vector.fromList
      . map (\Portal{portalCoordinates=Polygon pc,portalLinksTo=RoomKey plt} -> M.Object $ fromList [("coords", M.String . pack $ flatCoords pc), ("href", M.String . pack $ show plt)])
      $ roomObjPortals room
    flatCoords :: [(Int, Int)] -> String
    flatCoords coords = intercalate "," $ foldl (\acc (x,y) -> show x:show y:acc) [] coords


-- FIXME: better more accurate docs.
-- | The goal is to make certain values from the config available as mustache template variables,
-- by transforming the type into a list of substitutions.
appEnvConfigToSubstitutions :: Conf.AppEnvConfig -> [(T.Text, M.Value)]
appEnvConfigToSubstitutions appEnvConfig' =
  -- NOTE: I feel like this pattern of not making the keys in the substitution a M.String is rather weird... diverting
  -- the type for later plumbing... FIXME
  --
  -- FIXME: type Substitutions = [(M.String, M.Value)]
  -- Make sure above isn't already a part of the mustache framework in some way...
  [(k, M.String v) | (k, lookupFunc) <- Conf.appEnvConfigWhitelist, (Just v) <- [lookupFunc appEnvConfig']]


-- FIXME: extremely related to above, just bundle together?!
--parseMustacheChildWithConfigVars :: [FilePath] -> FilePath -> [(T.Text, Value)] -> ...
parseMustacheChildWithConfigVars :: [FilePath] -> FilePath -> [(T.Text, M.Value)] -> IO T.Text
parseMustacheChildWithConfigVars searchPath someFilePath substitutions =
  Conf.getAppEnvConfig >>= parseMustacheChild searchPath someFilePath . (substitutions ++) . appEnvConfigToSubstitutions


-- FIXME: this is similar to staticCopy! could abstract more. also this currently doesn't account for directoreis need to make. subdirs...
-- | Use Mustache renderer to generate files found in a particular directory, out to the built directory.
mustacheBuildThese :: IO [FilePath]
mustacheBuildThese = do
  allFilesInBuildThesePath <- map (drop (length mustacheBuildThesePath + 1)) <$> getFilesRecursive mustacheBuildThesePath
  -- FIXME: won't this not give the full path?
  traverse mustacheBuild allFilesInBuildThesePath


-- | Copy everything that must only be copied, not parsed in any way.
--
-- Returns what was copied to the built directory.
staticCopy :: IO [FilePath]
staticCopy = do
  allFilesInCopyDir <- getFilesRecursive copyPath
  traverse copyFunc allFilesInCopyDir
 where

  copyFunc :: FilePath -> IO FilePath
  copyFunc src = do
    let
      dest = buildPath </> drop (length copyPath + 1) src
      destDir = takeDirectory dest

    directoryExists <- doesDirectoryExist destDir
    _ <- if directoryExists
      then pure ()
      else createDirectoryIfMissing True destDir

    _ <- copyFile src dest
    pure dest


-- | Add the background image for a room.
--
-- The third argument (`String`) is the file name (not path).
--
-- Gives back the file path to the created image.
createRoomImage :: RoomUUID -> BSL.ByteString -> String -> IO FilePath
createRoomImage roomUUID fileContent fileName = do
  -- First write to the fs database (for room images)
  let
    -- The directory for this room in the filesystem database of images.
    roomImageDbDirectory = roomImagesPath </> show roomUUID
    -- The path (including filename) the image will be written to in the
    -- fs database.
    roomImageDbFilePath = roomImageDbDirectory </> fileName
  _ <- createDirectoryIfMissing True roomImageDbDirectory
  _ <- BSL.writeFile roomImageDbFilePath fileContent

  -- Now we copy from the fs database to the built directory
  let
    -- The directory this room gets built to, and where the image will
    -- get copied to from the database.
    roomDirectory = joinPath [buildPath, "rooms", show roomUUID]
    -- The path (including filename) the image will be copied to
    -- for the building process and in built.
    builtFilePath = roomDirectory </> fileName
  _ <- createDirectoryIfMissing True roomDirectory
  _ <- copyFile roomImageDbFilePath builtFilePath

  pure builtFilePath


-- | Create the static file and directory and everything for a provided room.
--
-- Gives back a relative path to the new room's index inside the directory root for `buildPath`.
createNewRoom :: RoomUUID -> Room -> [Portal] -> IO FilePath
createNewRoom uuid room portals = do
  let
    -- FIXME: this is terrible needs to be updated!
    templateName = "room.html"
    -- room object for mustache templates
    roomObj = RoomObject
      { roomObjId = uuid
      , roomObjPortals = portals
      , roomObjTitle = roomTitle room
      , roomObjDescription = roomDescription room
      , roomObjImagePath = roomBgFileName room
      }
    roomUuid = show uuid
    -- This is sloppy FIXME
    relativeRoomDirectoryPath = joinPath ["rooms", roomUuid]
    roomDirectory = joinPath [buildPath, relativeRoomDirectoryPath]
    roomIndexFilePath = joinPath [roomDirectory, "index.html"]
    substitutions = [("room", toMustache roomObj)] -- FIXME: feels sloppy?
  -- the joinPath on this line is extremely misleading, need to fix this.
  -- FIXME: should add config info from a config file for stuff like "site name", make a boilerplate function basically
  roomFileText <- parseMustacheChildWithConfigVars searchSpace templateName substitutions
  _ <- createDirectoryIfMissing True roomDirectory
  _ <- writeFile roomIndexFilePath . unpack $ roomFileText
  pure relativeRoomDirectoryPath


-- RETIRE THIS
-- | Create the admin backend page.
createAdminBackend :: IO FilePath
createAdminBackend = do
  let adminFileName = "admin.html"
  adminPageText <- parseMustacheChildWithConfigVars searchSpace adminFileName []
  let outPath = buildPath </> adminFileName
  _ <- writeFile outPath $ unpack adminPageText
  pure outPath


-- | Build out a file in the Mustache search path out to the build path.
mustacheBuild :: FilePath -> IO FilePath
mustacheBuild  fileName = do
  pageText <- parseMustacheChildWithConfigVars searchSpace fileName []
  let outPath = buildPath </> fileName
  _ <- writeFile outPath $ unpack pageText
  pure outPath


-- FIXME: this is SO messy!
-- FIXME: this is weird because it's the only one that hits the database...
-- refactor? should it not even get usernames? The output type is weird.
-- | Build author profile pages.
buildProfilePages :: ActionT Middle.Error ConfigM [FilePath]
buildProfilePages = do
  _ <- liftIO $ createDirectoryIfMissing True usersDirectory
  usernameSlugPairs <- getUsernameSlugPairs
  (generatedProfilePaths :: [FilePath]) <- liftIO $ traverse generateProfile usernameSlugPairs
  pure generatedProfilePaths
 where
  getUserRooms :: (MonadTrans t, MonadIO (t ConfigM))
    => RoomUUID
    -> t ConfigM [T.Text]
  getUserRooms userId = do
    (rooms :: [DB.Entity Room]) <- Middle.runDB $ DB.selectList [RoomAuthor DB.==. AccountKey userId] []
    case rooms of
      [] -> pure []
      ens -> do
        -- FIXME: head is bad!
        let roomIds = fmap (\(DB.Entity accountId _) -> head $ keyToValues accountId) ens
            roomUuids = fmap (\(PersistText id') -> id') roomIds
        pure roomUuids

  getUsernameSlugPairs :: ActionT Middle.Error ConfigM [(T.Text, T.Text, [T.Text])]
  getUsernameSlugPairs = do
    -- FIXME/NOTE: how to only select username?
    (accounts :: [DB.Entity Account]) <- Middle.runDB (DB.selectList [] [])
    -- FIXME: redundant
    case accounts of
      (_ :: DB.Entity Account):_ -> do
        traverse createTriplet accounts
      [] -> pure []

  -- TODO/FIXME: last value needs to be a RoomUUID
  createTriplet :: DB.Entity Account -> ActionT Middle.Error ConfigM (T.Text, T.Text, [T.Text])
  createTriplet accountEntity = do
    -- FIXME/NOTE: I hate this. This is so ugly. Is this really the best Persist can do for giving me
    -- the value of the id pertaining to the database entity?
    let (DB.Entity accountId account) = accountEntity
        authorId = case keyToValues accountId of
          [PersistLiteral_ Escaped authorId'] -> authorId'
          pv -> error . show $ pv -- FIXME
        authorUuid = RoomUUID <$> fromJust $ UUID.fromString (BSU.toString authorId)
    rooms <- getUserRooms authorUuid -- FROMJUST BAD FIXME
    pure (accountUsername account, accountUsername account, rooms)

  usersDirectory :: FilePath
  usersDirectory = buildPath </> "users"

  -- FIXME: it'd be better to have type text -> text -> io filepath
  generateProfile :: (T.Text, T.Text, [T.Text]) -> IO FilePath
  generateProfile (username, usernameSlug, rooms) = do
    let pathOut = usersDirectory </> T.unpack usernameSlug <> ".html"
    pageText <- parseMustacheChildWithConfigVars
      searchSpace
      "profile.html"
      [ ("username", MTypes.String username)
      , ("usernameSlug", MTypes.String usernameSlug)
      , ("rooms", MTypes.Array . Vector.fromList $ fmap MTypes.String rooms)
      ]
    _ <- writeFile pathOut $ unpack pageText
    pure pathOut


-- TODO: Create entire site from DB. Also copies essential static files...
-- ...

-- TODO/FIXME: rename to setup static? I don't know how to separate this from the traverse style logic of just building
-- the manually specified files... although that should eventually glob.
-- | Copy over the essential files to built/ from static.
--
-- Returns the paths to the various files built (relative to the `buildPath`).
--
-- Maybe the return value doesn't make sense.
setupEssentials :: IO [FilePath]
setupEssentials = do
  --filePaths <- traverse copyStatic staticFilesToCopy
  filePathsFromCopying <- staticCopy
  filePathsFromBuilding <- mustacheBuildThese
  adminPath <- createAdminBackend
  pure $ adminPath : filePathsFromCopying ++ filePathsFromBuilding
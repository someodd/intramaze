-- | Builds the static site from the database and other static site generator related things.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module PinkSands.Static (createRoomImage, createNewRoom, setupEssentials) where

import qualified Data.Vector as Vector (fromList)
import Data.List (intercalate)
import Data.HashMap.Strict ( fromList )
import Data.Text (unpack, pack)
import qualified Text.Mustache.Types as M (Value(..))
import PinkSands.Models (RoomUUID, Room(..), Portal(..), Key (RoomKey), Polygon (Polygon))
import qualified Data.ByteString.Lazy as BSL (writeFile, ByteString)
import System.FilePath (joinPath, (</>), takeDirectory)
import System.Directory (createDirectoryIfMissing, copyFile, doesDirectoryExist)
import Template
import System.Directory.Recursive (getFilesRecursive)
import Text.Mustache (ToMustache (toMustache))
import qualified Data.HashMap.Strict as HashMap
--import Control.Monad (filterM)
import qualified Data.Text as T


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


-- | Mustache search space. Mustache will look for includes and templates in general here.
searchSpace :: [FilePath]
searchSpace = [staticPath </> "mustache"]


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
  roomFileText <- parseMustacheChild searchSpace templateName substitutions
  _ <- createDirectoryIfMissing True roomDirectory
  _ <- writeFile roomIndexFilePath . unpack $ roomFileText
  pure relativeRoomDirectoryPath


-- | Create the admin backend page.
createAdminBackend :: IO FilePath
createAdminBackend = do
  let adminFileName = "admin.html"
  adminPageText <- parseMustacheChild searchSpace adminFileName []
  let outPath = buildPath </> adminFileName
  _ <- writeFile outPath $ unpack adminPageText
  pure outPath


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
  filePaths <- staticCopy
  adminPath <- createAdminBackend
  pure $ adminPath : filePaths
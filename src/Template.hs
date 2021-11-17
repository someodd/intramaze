-- | Templating system making heavy use of Mustache.
--
-- The purpose of this system is to have a Mustache templating system which
-- also allows for Frontmatter that can specify a parent template. If a
-- parent template is specified, the template will be inserted into {{child}}
-- of the parent template.
--     Frontmatter can also be used to specify Mustache substitutions/variables,
-- which themselves have Mustache substitutions in them (but not includes). If you
-- use a substitution in a Frontmatter entry, it's important that it is inside
-- quotations, otherwise the Frontmatter parser will fail:
--
--     title: "{{id}}"
--
{-# LANGUAGE OverloadedStrings #-}
module Template
    ( instanceLocationHack
    , parseMustacheChild
    ) where

import qualified Data.HashMap.Strict as H
import Text.Mustache (substitute, Template (partials, ast), compileTemplate)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Data.Maybe (catMaybes, mapMaybe, fromJust, fromMaybe)
import qualified Data.ByteString as BS
import Data.Frontmatter (IResult (Fail, Done, Partial), parse, frontmatterYaml)
import Data.Text.Encoding as TSE ( decodeUtf8 )
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HL
import Data.Text (Text)
import Data.Aeson (FromJSON, parseJSON)
import Text.Mustache.Compile (getPartials, cacheFromList)
import Data.Bool (bool)
import Text.Mustache.Types (toMustache)
import qualified Text.Mustache.Types as M (Value(..))
import qualified Data.Text.Encoding as TE
--import qualified Data.Bifunctor


newtype AesonMustache = AesonMustache M.Value


instance FromJSON AesonMustache where
  parseJSON a = pure $ AesonMustache $ toMustache a


-- | The entire reason I'm using `AesonMustache` at all is because I cannot
-- implement `FromJSON M.Value` in this module, it'll give me an error. So
-- I'm using this workaround.
instanceLocationHack :: HL.HashMap T.Text AesonMustache -> HL.HashMap T.Text M.Value
instanceLocationHack = HL.map (\(AesonMustache v) -> v)


-- | Get the Frontmatter from the provided `ByteString`, also returning the
-- rest of the document which proceedes the Frontmatter content.
getInstructions :: BS.ByteString -> Maybe (Text, HL.HashMap T.Text AesonMustache)
getInstructions someInput = --parseYamlFrontmatterMaybe
  case parse frontmatterYaml someInput of
    Done ri fm -> Just (TSE.decodeUtf8 ri, fm)
    Partial _ -> error "partial"
    Fail _ _ _ -> Nothing
    --Fail {} -> Nothing


-- FIXME: this isn't quite right! the replacements are a bit fucked... should pass [(text, a)] oh i am...
-- i wanted to use ToMustache a, but then I'd be limited to only providing the same thing over and over and that sucks.
-- FIXME/NOTE: do we actually want filePath to just use getFile in searchSpace?
-- | By default, Mustache does not support extension templates. So if you wanted
-- to embed one page in a default template it'd be tricky or annoying. This allows
-- for template extensions to be made. 
parseMustacheChild
  :: [FilePath]
  -> FilePath
  -> [(Text, M.Value)]
  -> IO Text
parseMustacheChild searchSpace filePath substitutions' = do
  -- Try and get the child template as Text or error.
  childText <- fromMaybe (error $ "cannot find in mustache search space: " ++ filePath) <$> getFile searchSpace filePath
  let childByteString = TE.encodeUtf8 childText
  --let substitutions' = map (Data.Bifunctor.second toMustache) substitutions'' -- FIXME

  -- this could be a bit cleaner FIXME
  case getInstructions childByteString of
    Just (childTextNoFrontmatter, instructions') -> do
      let
        instructions = instanceLocationHack instructions'
        substitutions = HL.toList instructions ++ substitutions'
      maybeParentText <- getFile searchSpace (let (M.String s) = fromJust $ HL.lookup "parent" instructions in T.unpack s)
      case maybeParentText of
        Nothing -> plainRender childText
        Just parentText -> do
          parentTemplate <- neoTextCompile searchSpace parentText
          -- done twice to do replacements on the replacements like "head" and the child template
          -- FIXME: this keeps having to be done to be thorough... should keep going until none left? or encounter can't replace?
          let parentRendered = substitute parentTemplate $ H.fromList $ ("child", M.String childTextNoFrontmatter) : substitutions
          substituted <- flip substitute (H.fromList substitutions) <$> neoTextCompile searchSpace parentRendered
          -- in case a child references its yaml which has mustache in it... FIXME this is lazy code
          flip substitute (H.fromList substitutions) <$> neoTextCompile searchSpace substituted

    Nothing -> do
      -- should just use render/parse or something instead? FIXME
      plainRender childText
  where
   plainRender text = flip substitute (H.fromList [] :: H.HashMap T.Text M.Value) <$> neoTextCompile searchSpace text


-- FIXME: would be even better if you could override the template cache for not
-- getting the same partials more than once. do a lookup in the hashmap of the templatecache,
-- if not found then can go through the process of getting it and adding it to the cache.
neoTextCompile :: [FilePath] -> T.Text -> IO Template
neoTextCompile searchSpace templateText = do
  -- FIXME: what if Left
  let (Right template) = compileTemplate "templateName" templateText
  partialsGotten <- catMaybes <$> traverse magicFunc (getPartials $ ast template)
  pure $ template {partials = cacheFromList $ mapMaybe (either (const Nothing) Just . uncurry compileTemplate) partialsGotten}
 where
  -- FIXME: could be simpler
  magicFunc :: FilePath -> IO (Maybe (FilePath, T.Text))
  magicFunc partialName = do
    (>>= \txt -> Just (partialName, txt)) <$> getFile searchSpace partialName


getFile :: [FilePath] -> FilePath -> IO (Maybe Text)
getFile [] _ = pure Nothing
getFile (templateDir : xs) fp = do
  doesFileExist filePath >>=
    bool
      (getFile xs fp)
      (Just . T.pack <$> readFile filePath)
 where
  filePath = templateDir </> fp
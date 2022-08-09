{-
This was copied from: https://gist.github.com/and-pete/ccaf10ceaf7d9544a74f5b4c3bea510f#file-scottyexample-hs-L336-L462

I'll give better credit soon...

I included this instead of listing it as a dependency because it's broken in nixpkgs and it's tiny, anyway.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

{-# LANGUAGE UndecidableInstances #-}

module Slug where

import Prelude

import "base" Control.Monad ( (>=>) )
import "exceptions" Control.Monad.Catch
    ( Exception(displayException), MonadThrow(..) )
import "exceptions" Control.Monad.Catch qualified as E
import "aeson" Data.Aeson
    ( FromJSON(parseJSON), ToJSON(toJSON), Value )
import "aeson" Data.Aeson qualified as Aeson
import "aeson" Data.Aeson.Types qualified as Aeson
import "base" Data.Char qualified as Char
import "base" Data.Data ( Data )
import "text" Data.Text ( Text )
import "text" Data.Text qualified as T
import "http-api-data" Web.HttpApiData
    ( FromHttpApiData(parseUrlPiece), ToHttpApiData(toUrlPiece) )

--------------------------------------------------------------------------------
-- Everything below here is just a direct copy from 'mrkkrp/slug-0.1.7' itself

--------------------------------------------------------------------------------
-- | From the archived 'mrkkrp/slug-0.1.7'
--------------------------------------------------------------------------------

-- | This exception is thrown by 'mkSlug' when its input cannot be converted
-- into a proper 'Slug'.

data SlugException
  = InvalidInput Text  -- ^ Slug cannot be generated for given text
  | InvalidSlug  Text  -- ^ Input is not a valid slug, see 'parseSlug'
  | InvalidLength Int  -- ^ Requested slug length is not a positive number
 deriving stock ( Show, Eq )

instance E.Exception SlugException where
  displayException (InvalidInput txt) = "Cannot build slug for " ++ show txt
  displayException (InvalidSlug  txt) = "The text is not a valid slug " ++ show txt
  displayException (InvalidLength n)   = "Invalid slug length: " ++ show n

-- | Slug. Textual value inside is always guaranteed to have the following
-- qualities:
--
--     * it's not empty;
--     * it consists only of alpha-numeric groups of characters (words)
--     separated by @\'-\'@ dashes in such a way that entire slug cannot
--     start or end in a dash and also two dashes in a row cannot be found;
--     * every character with defined notion of case is lower-cased.
--
-- Slugs are good for semantic URLs and also can be used as identifier of a
-- sort in some cases.

newtype Slug = Slug Text
  deriving newtype ( Eq, Ord )
  deriving stock ( Show, Data )

instance Semigroup Slug where
  x <> y = Slug (unSlug x <> "-" <> unSlug y)

-- | Create a 'Slug' from a 'Text' value, all necessary transformations are
-- applied. The argument of this function can be title of an article or
-- something like that.
--
-- Note that the result is inside 'MonadThrow', that means you can just get
-- it in 'Maybe', in more complex contexts it will throw 'SlugException'
-- exception using 'InvalidInput' constructor.
--
-- This function also has a useful property:
--
-- > mkSlug = mkSlug >=> mkSlug . unSlug

mkSlug :: MonadThrow m => Text -> m Slug
mkSlug txt =
  let ws = getSlugWords txt
  in if null ws
     then throwM (InvalidInput txt)
     else return . Slug . T.intercalate "-" $ ws

-- | Get textual representation of a 'Slug'.

unSlug :: Slug -> Text
unSlug (Slug x) = x

-- | Convert 'Text' to a possibly empty collection of words. Every word is
-- guaranteed to be non-empty alpha-numeric lower-cased sequence of
-- characters.

getSlugWords :: Text -> [Text]
getSlugWords = T.words . T.toLower . T.map f . T.replace "'" ""
  where
    f x = if Char.isAlphaNum x then x else ' '

-- | Convert a 'Text' into a 'Slug' only when it is already valid slug.
--
-- This function can throw the 'SlugException' exception using 'InvalidSlug'
-- constructor.

parseSlug :: MonadThrow m => Text -> m Slug
parseSlug v = mkSlug v >>= check
  where
    check s =
      if unSlug s == v
        then return s
        else throwM (InvalidSlug v)

-- | Ensure that given 'Slug' is not longer than given maximum number of
-- characters. If truncated slug ends in a dash, remove that dash too. (Dash
-- at the end would violate properties described in documentation for
-- 'Slug'.)
--
-- If the first argument is not a positive number, 'SlugException' is thrown
-- using 'InvalidLength' constructor.

truncateSlug :: MonadThrow m
  => Int               -- ^ Maximum length of slug, must be greater than 0
  -> Slug              -- ^ Original non-truncated slug
  -> m Slug            -- ^ Truncated slug
truncateSlug n v
  | n < 1     = throwM (InvalidLength n)
  | otherwise = mkSlug . T.take n . unSlug $ v


instance Read Slug where
  readsPrec n = (readsPrec n :: ReadS Text) >=> f
    where f (s, t) = (\x -> (x, t)) <$> parseSlug s

instance ToJSON Slug where
  toJSON :: Slug -> Value
  toJSON = toJSON . unSlug

instance FromJSON Slug where
  parseJSON :: Value -> Aeson.Parser Slug
  parseJSON = Aeson.withText "Slug" $ \txt ->
    case parseSlug txt of
      Left err -> fail (show err)
      Right val -> return val

instance ToHttpApiData Slug where
  toUrlPiece :: Slug -> Text
  toUrlPiece = unSlug

instance FromHttpApiData Slug where
  parseUrlPiece :: Text -> Either Text Slug
  parseUrlPiece = either (Left . T.pack . f) Right . parseSlug
    where
      f = E.displayException
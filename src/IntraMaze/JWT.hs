{-|
Module      : JWT
Description : JSON Web Token system for authenticating IntraMaze REST API requests.

= High-level overview

Code lifted in part from [the official libjwt-typed documentation](https://github.com/marcin-rzeznicki/libjwt-typed).
Great documentation, a highly suggested read.

Uses private key for signatures in order to enable verifying that the JWT
user is who they say they are.

The "JsonRequests" module makes use of this JWT system.

= About JWTs

JSON Web Tokens are broken down like so: header.payload.signature. For our
purposes, JWT is used to verify certain things about the requester, like that
they are who they say they are, and what permissions they have, without
requiring conventional login sessions, or requiring that they send their
credentials with each rest API request.

The header portion of the JWT indicates things about the JWT, like the signing
algorithm being used and the type of the token (which is JWT). This is then
Base64Url encoded.

The next part is the payload, which contains the claims ('UserClaims'). The
payload is then Base64Url encoded.

Finally the signature: you take the encoded header, the encoded payload, a
secret, the algorithm specified in the header, and sign all of that. The
signature verifies the message hasn't been tampered with, and when signed with a
private key, it verifies that the sender of the JWT is who they say they are.

JWTs should be sent by as an HTTP header by the requester in this format:

  Authorization: Bearer <token>

For more info on JWT: <https://jwt.io/introduction>
-}
-- FIXME: Note that if you send JWT tokens through HTTP headers, you should try to prevent them from getting too big. Some servers don't accept more than 8 KB in headers.
-- FIXME: could use Text instead of ByteString!
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-} -- just for sweet and short examples
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module IntraMaze.JWT (decodeAndValidateFull, UserClaims(..), makeToken) where

import           Web.Libjwt
import           Control.Arrow                  ( left )
import           Control.Exception              ( catch
                                                , displayException
                                                )
import           Data.Either.Validation         ( validationToEither )


import           Control.Monad.Time             (MonadTime)
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString as BS
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import GHC.Generics ( Generic )

import           Prelude                 hiding ( exp )
import qualified Data.Aeson as A


-- | The key pair used for JWT signatures.
--
-- Must be ECDSA with curve P-256.
ecP521KeyPair :: IO EcKeyPair
ecP521KeyPair = do
  private <- BS.readFile "jwt-priv-sig-key.pem"
  public <- BS.readFile "jwt-pub-sig-key.pem"
  pure $ FromEcPem { ecPrivKey = private, ecPubKey = public }


ecdsa512 :: IO (Algorithm EcKeyPair)
ecdsa512 = fmap ECDSA256 ecP521KeyPair


-- | The issuer is hardcoded because it's assumed there's only one issuer. So
-- there's no complex issuer/key lookup system. For similar reasons, audience is
-- ignored.
issuer :: String
issuer = "https://myApp.com"


-- | The "payload" portion of the JWT. Used for representing the JWT payload.
data UserClaims = UserClaims
  { userId :: UUID
  -- ^ Verified/authenticated UUID of the user/requestor.
  , userName :: Text
  -- ^ Same as `userId`, but the actual username.
  , isRoot :: Bool
  -- ^ Does this user have administrator privileges?
  }
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON)  -- TODO: this might be bad. only does this for whoami check. need to read more thoroughly into private and public claims etc.


-- | This is used for decoding a JWT.
--
-- I wish somehow this could be generated automatically from 'UserClaims'.
--
-- See: <https://hackage.haskell.org/package/libjwt-typed-0.2/docs/Libjwt-Payload.html>
type MyJwt = Jwt
  '["userId" ->> UUID, "userName" ->> Text, "isRoot" ->> Bool]
  -- ^ Payload
  'NoNs
  -- ^ Namespace


-- | Decode a JWT and validate that it's intended for us and not expired.
decodeAndValidateFull :: IO ByteString -> IO (Either String UserClaims)
decodeAndValidateFull token =
    ( left (("Token not valid: " ++) . show)
    . fmap toUserClaims
    . validationToEither
    <$> decodeAndValidate token
    )
    `catch` onError
 where
  toUserClaims = fromPrivateClaims . privateClaims . payload . getValid

  onError (e :: SomeDecodeException) =
    return $ Left $ "Cannot decode token " ++ displayException e

  decodeAndValidate :: IO ByteString -> IO (ValidationNEL ValidationFailure (Validated MyJwt))
  decodeAndValidate token' = do
    decodeWithThis <- ecdsa512
    jwtFromByteString settings mempty decodeWithThis =<< token'
   where settings = Settings { leeway = 5, appName = Just issuer }


makeToken :: UUID -> Text -> Bool -> IO ByteString -- or any other MonadTime instance
makeToken userId' username rootStatus = do
  signWithThis <- ecdsa512
  getToken . sign signWithThis <$> mkPayload userId' username rootStatus


instance ToPrivateClaims UserClaims
instance FromPrivateClaims UserClaims


mkPayload
  :: MonadTime m
  => UUID
  -> Text
  -> Bool
  -- FIXME: nonempty
  -> m (Payload '[ 'Grant "userId" UUID, 'Grant "userName" Text,
                   'Grant "isRoot" Bool]
        'NoNs)
mkPayload userId' username rootStatus = jwtPayload
  -- FIXME: use envvar for recipient and config in general and also issuer name.
  (withIssuer "myApp" <> withRecipient issuer <> setTtl 300)
  UserClaims { userId    = userId'
             , userName  = username
             , isRoot    = rootStatus
             }
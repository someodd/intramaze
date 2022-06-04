{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-} -- just for sweet and short examples
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module IntraMaze.JWT where

import           Web.Libjwt
import           Control.Arrow                  ( left )
import           Control.Exception              ( catch
                                                , displayException
                                                )
import           Data.Either.Validation         ( validationToEither )


import           Control.Monad.Time             (MonadTime)
import           Data.ByteString                ( ByteString )
--import           Data.Default
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( UUID )
import           GHC.Generics

import           Prelude                 hiding ( exp )
import qualified Crypto.KDF.Argon2       as Argon2
import Crypto.Error
import qualified Data.ByteString as BS
import qualified Data.Aeson as A


type Token = ByteString

-- FIXME: make an envvar a part of getConfig in IntraMaze.hs
-- | The site-wide secret pepper used to salt password hashes, in conjunction with the per-user salt.
-- It is important to note that in the future this will be a part of the configuration specification
-- as seen in IntraMaze.hs.
globalPepper :: BS.ByteString 
globalPepper = "hwaL*C4<U#2-0R5%~]B.Vd/+&$5ofxXQ^"


-- | Hash a password.
hash :: ByteString -> ByteString -> ByteString -> CryptoFailable BS.ByteString
hash theGlobalPepper usersSalt password =
  Argon2.hash Argon2.defaultOptions password (theGlobalPepper <> usersSalt) 10


-- | Verify that the provided password hashes to the same value (considering the global pepper and the user's salt)
-- as the provided hashed password.
verifyPassword :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Bool 
verifyPassword theGlobalPepper usersCorrectHash usersSalt attemptedPassword =
   case hash theGlobalPepper usersSalt attemptedPassword of
    CryptoPassed bs -> bs == usersCorrectHash  -- FIXME: bs is bytestring, not text! maybe model should account for that too.
    CryptoFailed _ -> False


-- FIXME: UUID should be RowUUID (which needs to be renamed also...)
data UserClaims = UserClaims { userId :: UUID
                             , userName :: Text
                             , isRoot :: Bool
                             , createdAt :: UTCTime
                             , accounts :: NonEmpty UUID
                             }
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON)



type MyJwt
  = Jwt
      '["userId" ->> UUID, "userName" ->> Text, "isRoot" ->> Bool, "createdAt" ->> UTCTime, "accounts" ->> NonEmpty UUID]
      'NoNs

--decodeDoNotUse :: IO (Decoded MyJwt)
--decodeDoNotUse = decodeByteString hmac512 =<< token

-- FIXME: should not be IO ByteString
decodeAndValidate :: IO ByteString -> IO (ValidationNEL ValidationFailure (Validated MyJwt))
decodeAndValidate token = jwtFromByteString settings mempty hmac512 =<< token
  where settings = Settings { leeway = 5, appName = Just "https://myApp.com" }


decodeAndValidateFull :: IO ByteString -> IO (Either String UserClaims)
decodeAndValidateFull token =
  (   left (("Token not valid: " ++) . show)
    .   fmap toUserClaims
    .   validationToEither
    <$> decodeAndValidate token
    )
    `catch` onError
 where
  toUserClaims = fromPrivateClaims . privateClaims . payload . getValid
  onError (e :: SomeDecodeException) =
    return $ Left $ "Cannot decode token " ++ displayException e

{-
λ> decodeAndValidate 
Success (MkValid {getValid = Jwt {header = Header {alg = HS512 (MkSecret {reveal = "MjZkMDY2OWFiZmRjYTk5YjczZWFiZjYzMmRjMzU5NDYyMjMxODBjMTg3ZmY5OTZjM2NhM2NhN2MxYzFiNDNlYjc4NTE1MjQxZGI0OWM1ZWI2ZDUyZmMzZDlhMmFiNjc5OWJlZTUxNjE2ZDRlYTNkYjU5Y2IwMDZhYWY1MjY1OTQgIC0K"}), typ = JWT}, payload = ClaimsSet {iss = Iss (Just "myApp"), sub = Sub Nothing, aud = Aud ["https://myApp.com"], exp = Exp (Just (NumericDate {secondsSinceEpoch = 1599504161})), nbf = Nbf Nothing, iat = Iat (Just (NumericDate {secondsSinceEpoch = 1599503861})), jti = Jti Nothing, privateClaims = (#userId ->> 5a7c5cdd-3909-456b-9dd2-6ba84bfeeb25, #userName ->> "JohnDoe", #isRoot ->> False, #createdAt ->> 2020-07-31 11:45:00 UTC, #accounts ->> (0bdf91cc-48bb-47f5-b633-920c34bd2352 :| []))}}})
 -}



makeToken :: UUID -> Text -> Bool -> IO ByteString -- or any other MonadTime instance
makeToken userId username rootStatus = getToken . sign hmac512 <$> mkPayload'' userId username rootStatus
{-
λ> token
"eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhY2NvdW50cyI6WyIwYmRmOTFjYy00OGJiLTQ3ZjUtYjYzMy05MjBjMzRiZDIzNTIiXSwiYXVkIjpbImh0dHBzOi8vbXlBcHAuY29tIl0sImNyZWF0ZWRBdCI6IjIwMjAtMDctMzFUMTE6NDU6MDBaIiwiZXhwIjoxNTk5NDk5MDczLCJpYXQiOjE1OTk0OTg3NzMsImlzUm9vdCI6ZmFsc2UsImlzcyI6Im15QXBwIiwidXNlcklkIjoiNWE3YzVjZGQtMzkwOS00NTZiLTlkZDItNmJhODRiZmVlYjI1IiwidXNlck5hbWUiOiJKb2huRG9lIn0.KH4YSODoTxuNLPYCyz0lmoVDHYJpvL8k6fccFugqs-6VcpctXeR4OYyWOZJDi294r6njCqRP15eqYpwrrzKKrQ" 
-}


hmac512 :: Algorithm Secret
hmac512 =
  HMAC512
    "MjZkMDY2OWFiZmRjYTk5YjczZWFiZjYzMmRjMzU5NDYyMjMxODBjMTg3ZmY5OTZjM2NhM2NhN2Mx\
    \YzFiNDNlYjc4NTE1MjQxZGI0OWM1ZWI2ZDUyZmMzZDlhMmFiNjc5OWJlZTUxNjE2ZDRlYTNkYjU5\
    \Y2IwMDZhYWY1MjY1OTQgIC0K"


instance ToPrivateClaims UserClaims
instance FromPrivateClaims UserClaims

mkPayload''
  :: MonadTime m
  => UUID
  -> Text
  -> Bool
  -- FIXME: nonempty
  -> m (Payload '[ 'Grant "userId" UUID, 'Grant "userName" Text,
                   'Grant "isRoot" Bool, 'Grant "createdAt" UTCTime,
                   "accounts" ->> NonEmpty UUID]
        'NoNs)
mkPayload'' userId username rootStatus = jwtPayload
  -- FIXME: use envvar for recipient and config in general and also issuer name.
  (withIssuer "myApp" <> withRecipient "https://myApp.com" <> setTtl 300)
  UserClaims { userId    = userId
             , userName  = username
             , isRoot    = rootStatus
             , createdAt = read "2020-07-31 11:45:00 UTC" -- FIXME
             , accounts  = read "0bdf91cc-48bb-47f5-b633-920c34bd2352" :| [] -- FIXME
             }
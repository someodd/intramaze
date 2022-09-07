{-# LANGUAGE OverloadedStrings #-}
import qualified Web.Scotty.Trans as Trans
import Network.Wai (Application)
import Test.Tasty.Wai
import Test.Tasty
import Data.ByteString.UTF8 as BSU

import           Interwebz (application, appToIO, apiRoute)
import qualified Web.Scotty as S
import Interwebz.Config (getConfig)


apiRoute' = BSU.fromString . apiRoute


-- TODO: initialize with SQL, like a `test-data.sql`
main :: IO ()
main = do
    c <- getConfig
    testApp <- appToIO c $ application c

    defaultMain $ testGroup "Tasty-Wai Tests"
        [
            testWai testApp "Hello to World" $ do
                res <- get $ apiRoute' "/rooms"
                assertBody "world!" res,

            testWai testApp "Echo to thee" $ do
                res <- post "echo" "thus"
                --assertStatus' H.status200 res -- Use functions from Network.HTTP.Types
                assertStatus 200 res          -- Use raw ints
                assertBody "thus" res
        ]
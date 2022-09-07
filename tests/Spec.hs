{-# LANGUAGE OverloadedStrings #-}
import qualified Web.Scotty.Trans as Trans
import Network.Wai (Application)
import Test.Tasty.Wai
import Test.Tasty

import           Interwebz (application, appToIO)
import qualified Web.Scotty as S
import Interwebz.Config (getConfig)


-- TODO: initialize with SQL, like a `test-data.sql`
main :: IO ()
main = do
    c <- getConfig
    testApp <- appToIO c $ application c

    defaultMain $ testGroup "Tasty-Wai Tests"
        [
            testWai testApp "Hello to World" $ do
                res <- get "/api/v0/rooms"
                assertBody "world!" res,

            testWai testApp "Echo to thee" $ do
                res <- post "echo" "thus"
                --assertStatus' H.status200 res -- Use functions from Network.HTTP.Types
                assertStatus 200 res          -- Use raw ints
                assertBody "thus" res
        ]
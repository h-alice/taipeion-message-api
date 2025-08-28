{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- test/LibSpec.hs
module TaipeiOnSpec (spec) where

import Test.Hspec
import TaipeiOn.Message
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Applicative (liftA)
import Data.Aeson ( encode )
import Data.Aeson.Decoding ( eitherDecodeStrict )
import Data.Text.Lazy.Encoding as TL ( decodeUtf8 )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


import Text.RawString.QQ
import qualified Data.Text.Encoding as T
import Data.Either (fromRight)



spec :: Spec
spec = do

  describe "message serialization" $ do
    let expectedBC = ("{\"ask\":\"broadcastMessage\",\"message\":{\"text\":\"test message\",\"type\":\"text\"}}" :: T.Text)
    let serializedBC = TL.toStrict $ TL.decodeUtf8 $ encode $ mkBroadcastMessage $ mkMessage "text" "test message"
    it "broadcast message" $ serializedBC `shouldBe` expectedBC

    let expectedPM = ("{\"ask\":\"sendMessage\",\"message\":{\"text\":\"cool message\",\"type\":\"text\"},\"recipient\":\"cool-dude\"}" :: T.Text)
    let serializedPM = TL.toStrict $ TL.decodeUtf8 $ encode $ mkPrivateMessage "cool-dude" $ mkMessage "text" "cool message"
    it "private message" $ serializedPM `shouldBe` expectedPM

  describe "message deserialization" $ do

    let testPayload = WebhookPayload
          {
              wpDestination = 180284
            , wpEvents = [
                MessageEvent {
                    mevType = "message"
                  , mevTimestamp = 1462629479859
                  , mevSource = MessageSource { msType = "user", msUserId = "taylor" }
                  , mevMessage = Message  { msgType = "text"
                                          , msgText = "Hello, world!"
                                          , msgId = "ecca2bfd-4d57-4acb-92ed-de38ab54bad1"
                                          }
                }

            ]
          }
    let payloadEmpty = WebhookPayload {
              wpDestination = 0 , wpEvents = []
    }

    -- Sample copied from development manual
    let test = [r|
      {
        "destination": 180284,
        
          "events": [
          {
            "type": "message",
            "timestamp": 1462629479859,
            "source": {
              "type": "user",
              "userId": "taylor"
            },
            "message": {
              "type": "text",
              "id": "ecca2bfd-4d57-4acb-92ed-de38ab54bad1",
              "text": "Hello, world!"
            }
          }
        ]
      }
    |] :: T.Text

    runIO $ putStrLn "Test JSON payload:"
    runIO $ print (T.encodeUtf8 $ T.strip test)
    runIO $ putStrLn "Decode result:"
    runIO $ print ((eitherDecodeStrict $ T.encodeUtf8 $ T.strip test ) :: Either String WebhookPayload)

    it "webhook payload" $ testPayload `shouldBe` fromRight payloadEmpty (eitherDecodeStrict $ T.encodeUtf8 $ T.strip test)
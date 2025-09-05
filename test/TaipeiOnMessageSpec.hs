{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- test/LibSpec.hs
module TaipeiOnMessageSpec (spec) where

import Test.Hspec
import TaipeiOn.Message
import Data.Aeson ( encode )
import Data.Text.Lazy.Encoding as TL ( decodeUtf8 )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

spec :: Spec
spec = do

  describe "text message serialization" $ do
    let expectedBC = ("{\"ask\":\"broadcastMessage\",\"message\":{\"text\":\"test message\",\"type\":\"text\"}}" :: T.Text)
    let serializedBC = TL.toStrict $ TL.decodeUtf8 $ encode $ mkBroadcastMessage $ mkTextMessage "test message"
    it "broadcast message" $ serializedBC `shouldBe` expectedBC

    let expectedPM = ("{\"ask\":\"sendMessage\",\"message\":{\"text\":\"cool message\",\"type\":\"text\"},\"recipient\":\"cool-dude\"}" :: T.Text)
    let serializedPM = TL.toStrict $ TL.decodeUtf8 $ encode $ mkPrivateMessage "cool-dude" $ mkTextMessage "cool message"
    it "private message" $ serializedPM `shouldBe` expectedPM

  describe "image message serialization" $ do
    let expectedBC = ("{\"ask\":\"broadcastMessage\",\"message\":{\"fileID\":\"some ID\",\"showFileName\":\"image.jpg\",\"text\":\"image msg\",\"type\":\"image\"}}" :: T.Text)
    let serializedBC = TL.toStrict $ TL.decodeUtf8 $ encode $ mkBroadcastMessage $ mkImageMessage (Just "image msg") "image.jpg" "some ID"
    it "broadcast message" $ serializedBC `shouldBe` expectedBC

    let expectedPM = ("{\"ask\":\"sendMessage\",\"message\":{\"fileID\":\"some ID\",\"showFileName\":\"image.jpg\",\"text\":\"image msg\",\"type\":\"image\"},\"recipient\":\"cool-dude\"}" :: T.Text)
    let serializedPM = TL.toStrict $ TL.decodeUtf8 $ encode $ mkPrivateMessage "cool-dude" $ mkImageMessage (Just "image msg") "image.jpg" "some ID"
    it "private message" $ serializedPM `shouldBe` expectedPM

  describe "audio message serialization" $ do -- Also tested the none field serialization
    let expectedBC = ("{\"ask\":\"broadcastMessage\",\"message\":{\"fileID\":\"some audio\",\"showFileName\":\"audio.wav\",\"type\":\"audio\"}}" :: T.Text)
    let serializedBC = TL.toStrict $ TL.decodeUtf8 $ encode $ mkBroadcastMessage $ mkAudioMessage Nothing "audio.wav" "some audio"
    it "broadcast message" $ serializedBC `shouldBe` expectedBC

    let expectedPM = ("{\"ask\":\"sendMessage\",\"message\":{\"fileID\":\"some audio\",\"showFileName\":\"audio.wav\",\"type\":\"audio\"},\"recipient\":\"cool-dude\"}" :: T.Text)
    let serializedPM = TL.toStrict $ TL.decodeUtf8 $ encode $ mkPrivateMessage "cool-dude" $ mkAudioMessage Nothing "audio.wav" "some audio"
    it "private message" $ serializedPM `shouldBe` expectedPM

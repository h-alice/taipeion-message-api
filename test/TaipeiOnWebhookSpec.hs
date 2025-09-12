{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TaipeiOnWebhookSpec (spec) where
import Test.Hspec
import qualified TaipeiOn.Webhook.Event as WE
import Data.Aeson.Decoding ( eitherDecodeStrict )
import qualified Data.Text as T
import Text.RawString.QQ
import qualified Data.Text.Encoding as T


mkTestUserMsg :: WE.MessageObject -> WE.WebhookPayload 
mkTestUserMsg msg = WE.WebhookPayload
  { WE.wpDestination = 87
  , WE.wpEvents =
    [ WE.MessageEvent
      { WE.mevType = "message"
      , WE.mevTimestamp = 1757654321
      , WE.mevSource = WE.EventSource
        { WE.esType = "user"
        , WE.esUserId = "some-user-id"
        }
      , WE.mevMessage = msg
      }
    ]
  }

spec :: Spec
spec = do

    let textMsg = WE.TextMsg  
                    ( WE.TextMessage
                      { WE.txtId = "ecca2bfd-4d57-4acb-92ed-de38ab54bad1"
                      , WE.txtText = "Hello, world!"
                      }
                    )

    -- Sample copied from development manual
    let whTextMsg = [r|
      {
        "destination": 87,
        
          "events": [
          {
            "type": "message",
            "timestamp": 1757654321,
            "source": {
              "type": "user",
              "userId": "some-user-id"
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

    --runIO $ putStrLn "Test JSON payload:"
    --runIO $ print (T.encodeUtf8 $ T.strip test)
    --runIO $ putStrLn "Decode result:"
    --runIO $ print ((eitherDecodeStrict $ T.encodeUtf8 $ T.strip test ) :: Either String WE.WebhookPayload)

    it "webhook text message deserialization" $ Right ( mkTestUserMsg textMsg )
              `shouldBe` eitherDecodeStrict (T.encodeUtf8 $ T.strip whTextMsg)


    let fileMsg = WE.FileMsg
                    ( WE.FileMessage
                      { WE.fmFileInfo = WE.FileInfo
                        { WE.fiName = "test_file.txt"
                        , WE.fiExtension = ".txt"
                        , WE.fiSize = 999
                        , WE.fiToken = "some-download-token"
                        }
                      }
                    )

    -- Sample from actual collected logs.
    let whFileMsg = [r|
      {
        "destination":87,
        "events":[
          {
            "type":"message", "timestamp":1757654321,
            "source":{"type":"user", "userId":"some-user-id"},
              "message":{
                "type":"file",
                "fileInfo":{
                  "fileName":"test_file.txt",
                  "fileExtension":".txt",
                  "fileSize":999,
                  "downloadToken":"some-download-token"
                }
              }
          }
        ]
      }
    |] :: T.Text

    it "webhook file message deserialization" $ Right ( mkTestUserMsg fileMsg )
          `shouldBe` eitherDecodeStrict (T.encodeUtf8 $ T.strip whFileMsg)


    let imgMsg = WE.ImageMsg
                    ( WE.ImageMessage
                      { WE.imPreviewImage = WE.PreviewImage
                        { WE.piToken = "preview-img-token"
                        }
                      , WE.imFileInfo = WE.FileInfo
                        { WE.fiName = "image.png"
                        , WE.fiExtension = ".png"
                        , WE.fiSize = 123456
                        , WE.fiToken = "some-token"
                        }
                      }
                    )

    -- Sample from actual collected logs.
    let whImgMsg = [r|
      {
        "destination":87,
        "events":[
          {
            "type":"message","timestamp":1757654321,
            "source":{"type":"user","userId":"some-user-id"},
            "message":{
              "type":"image",
              "previewImage":{"downloadToken":"preview-img-token"},
              "fileInfo":{
                "fileName":"image.png",
                "fileExtension":".png",
                "fileSize":123456,
                "downloadToken":"some-token"
              }
            }
          }
        ]
      }
    |] :: T.Text

    it "webhook image message deserialization" $ Right ( mkTestUserMsg imgMsg )
          `shouldBe` eitherDecodeStrict (T.encodeUtf8 $ T.strip whImgMsg)


    let vidMsg = WE.VideoMsg
                    ( WE.VideoMessage
                      { WE.viDuration = 3
                      , WE.viPreviewImage = WE.PreviewImage
                        { WE.piToken = "prev-token"
                        }
                      , WE.viFileInfo = WE.FileInfo
                        { WE.fiName = "Video.mp4"
                        , WE.fiExtension = ".mp4"
                        , WE.fiSize = 123456
                        , WE.fiToken = "some-vid-token"
                        }
                      }
                    )

    -- Sample from actual collected logs.
    let whVidMsg = [r|
      {
        "destination":87,
        "events":[
          {
            "type":"message","timestamp":1757654321,
            "source":{"type":"user","userId":"some-user-id"},
            "message":{
              "type":"video","duration":3,
              "previewImage":{"downloadToken":"prev-token"},
              "fileInfo":{
                "fileName":"Video.mp4",
                "fileExtension":".mp4",
                "fileSize":123456,
                "downloadToken":"some-vid-token"
              }
            }
          }
        ]
      }
    |] :: T.Text

    it "webhook video message deserialization" $ Right ( mkTestUserMsg vidMsg )
          `shouldBe` eitherDecodeStrict (T.encodeUtf8 $ T.strip whVidMsg)

    let audMsg = WE.AudioMsg
                    ( WE.AudioMessage
                      { WE.aiDuration = 1
                      , WE.aiFileInfo = WE.FileInfo
                        { WE.fiName = "Voice.mp3"
                        , WE.fiExtension = ".mp3"
                        , WE.fiSize = 5983
                        , WE.fiToken = "some-token"
                        }
                      }
                    )

    -- Sample from actual collected logs.

    let whAudMsg = [r|
      {
        "destination":87,
        "events":[
          {
            "type":"message","timestamp":1757654321,
            "source":{"type":"user","userId":"some-user-id"},
            "message":{
              "type":"audio","duration":1,
              "fileInfo":{
                "fileName":"Voice.mp3",
                "fileExtension":".mp3",
                "fileSize":5983,
                "downloadToken":"some-token"
              }
            }
          }
        ]
      }
    |] :: T.Text


    it "webhook audio message deserialization" $ Right ( mkTestUserMsg audMsg )
          `shouldBe` eitherDecodeStrict (T.encodeUtf8 $ T.strip whAudMsg)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- test/LibSpec.hs
module TaipeiOnApiPayloadSpec (spec) where

import Test.Hspec
import qualified TaipeiOn.Webhook.Event as WE

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

    let t = WE.WebhookPayload
              { WE.wpDestination = 180284
              , WE.wpEvents =
                [ WE.MessageEvent
                  { WE.mevType = "message"
                  , WE.mevTimestamp = 1462629479859
                  , WE.mevSource = WE.EventSource
                                    { WE.esType = "user"
                                    , WE.esUserId = "taylor"
                                    }
                                    , WE.mevMessage = WE.TextMsg  ( WE.TextMessage
                                                                    { WE.txtId = "ecca2bfd-4d57-4acb-92ed-de38ab54bad1"
                                                                    , WE.txtText = "Hello, world!"
                                                                    }
                                                                  )
                  }
                ]
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
    runIO $ print ((eitherDecodeStrict $ T.encodeUtf8 $ T.strip test ) :: Either String WE.WebhookPayload)

    it "webhook payload deserialization" $ Right t `shouldBe` eitherDecodeStrict (T.encodeUtf8 $ T.strip test)
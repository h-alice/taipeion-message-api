{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
  
-- src/TaipeiOn/Message.hs
module TaipeiOn.Message
    ( Message
    , MessageSource
    , MessageEvent
    , WebhookPayload
    , ChannelMessagePayload
    , mkMessage
    , mkBroadcastMessage
    , mkPrivateMessage
    ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Generics
import Data.Text (Text)

data Message = Message
  { msgType :: Text,
    msgId   :: Text,
    msgText :: Text
  }
  deriving (Show, Eq, Generic)

data MessageSource = MessageSource
  { msType   :: Text,
    msUserId :: Text
  }
  deriving (Show, Eq, Generic)

data MessageEvent = MessageEvent
  { mevType      :: Text,
    mevTimestamp :: Int,
    mevSource    :: MessageSource,
    mevMessage   :: Message
  }
  deriving (Show, Eq, Generic)

data WebhookPayload = WebhookPayload
  { wpDestination :: Int,
    wpEvents      :: [MessageEvent]
  }
  deriving (Show, Eq, Generic)

data ChannelMessagePayload = ChannelMessagePayload
  { cmpAsk       :: Text,
    cmpRecipient :: Text,
    cmpMessage   :: Message
  }
  deriving (Show, Eq, Generic)

instance FromJSON Message where
    parseJSON :: Value -> Parser Message
    parseJSON = withObject "Message" $ \v -> Message
        <$> v .:  "type" 
        <*> v .:? "id"    .!= ""
        <*> v .:? "text"  .!= ""

instance FromJSON MessageSource where
    parseJSON :: Value -> Parser MessageSource
    parseJSON = withObject "MessageSource" $ \v -> MessageSource
        <$> v .: "type"
        <*> v .: "userId"

instance FromJSON MessageEvent where
    parseJSON :: Value -> Parser MessageEvent
    parseJSON = withObject "MessageEvent" $ \v -> MessageEvent
        <$> v .: "type"
        <*> v .: "timestamp"
        <*> v .: "source"
        <*> v .: "message"

instance FromJSON WebhookPayload where
    parseJSON :: Value -> Parser WebhookPayload
    parseJSON = withObject "WebhookPayload" $ \v -> WebhookPayload
        <$> v .: "destination"
        <*> v .: "events"

instance ToJSON Message where
    toJSON :: Message -> Value
    toJSON Message{..} =
        object $ filter notEmpty  [  "id" .= msgId
                                  ,  "type" .= msgType
                                  ,  "text" .= msgText
                                  ]
                where 
                  notEmpty (_, v) = not (isEmpty v)

                  isEmpty (String s) = s == ""
                  isEmpty (Array  a) = null a
                  isEmpty Null       = True
                  isEmpty _          = False

instance ToJSON ChannelMessagePayload where
    toJSON :: ChannelMessagePayload -> Value
    toJSON ChannelMessagePayload{..} =
        object $ filter notEmpty [  "ask" .= cmpAsk
                                  , "recipient" .= cmpRecipient
                                  , "message" .= cmpMessage
                                  ]
                where 
                  notEmpty (_, v) = not (isEmpty v)

                  isEmpty (String s) = s == ""
                  isEmpty (Array  a) = null a
                  isEmpty Null       = True
                  isEmpty _          = False


-- Factory

-- Message to send
mkMessage :: Text -> Text -> Message
mkMessage msgType msgText = Message { msgType = msgType
                                    , msgId = ""
                                    , msgText = msgText }

mkBroadcastMessage :: Message -> ChannelMessagePayload
mkBroadcastMessage msg = ChannelMessagePayload  { cmpAsk = "broadcastMessage"
                                                , cmpRecipient = ""
                                                , cmpMessage = msg 
                                                }

mkPrivateMessage :: Text -> Message -> ChannelMessagePayload
mkPrivateMessage recipient msg = ChannelMessagePayload  
                                    { cmpAsk = "sendMessage"
                                    , cmpRecipient = recipient
                                    , cmpMessage = msg 
                                    }
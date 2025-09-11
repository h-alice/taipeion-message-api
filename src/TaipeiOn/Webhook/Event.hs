{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
  
-- src/TaipeiOn/Webhook/Event.hs
module TaipeiOn.Webhook.Event
    ( WebhookPayload(..)
    , MessageEvent(..)
    , EventSource(..)
    , MessageObject(..)
    , TextMessage(..)
    , ImageMessage(..)
    , VideoMessage(..)
    , AudioMessage(..)
    , FileMessage(..)
    , FileInfo(..)
    , PreviewImage(..)
    ) where

import Data.Aeson ( withObject
                  , (.:)
                  , FromJSON(parseJSON)
                  , Value(Object) 
                  )
import Data.Aeson.Types (Parser)
import GHC.Generics ( Generic )
import Data.Text (Text, unpack)

--
-- Data structures for incoming webhook events
--
  
data TextMessage = TextMessage
  { txtId   :: Text
  , txtText :: Text
  }
  deriving (Show, Eq, Generic)

data FileInfo = FileInfo
  { fiName      :: Text
  , fiExtension :: Text
  , fiSize      :: Int
  , fiToken     :: Text
  }
  deriving (Show, Eq, Generic)

data PreviewImage = PreviewImage
  { piToken :: Text
  }
  deriving (Show, Eq, Generic)

data ImageMessage = ImageMessage
  { imPreviewImage :: PreviewImage
  , imFileInfo     :: FileInfo
  }
  deriving (Show, Eq, Generic)

data VideoMessage = VideoMessage
  { viDuration     :: Int
  , viPreviewImage :: PreviewImage
  , viFileInfo     :: FileInfo
  }
  deriving (Show, Eq, Generic)

data AudioMessage = AudioMessage
  { aiDuration     :: Int
  , aiFileInfo     :: FileInfo
  }
  deriving (Show, Eq, Generic)

data FileMessage = FileMessage
  { fmFileInfo :: FileInfo
  }
  deriving (Show, Eq, Generic)

data MessageObject
  = TextMsg  TextMessage
  | ImageMsg ImageMessage
  | VideoMsg VideoMessage
  | AudioMsg AudioMessage
  | FileMsg  FileMessage
  deriving (Show, Eq)

data EventSource = EventSource
  { esType   :: Text
  , esUserId :: Text
  }
  deriving (Show, Eq, Generic)

data MessageEvent = MessageEvent
  { mevType      :: Text
  , mevTimestamp :: Int
  , mevSource    :: EventSource
  , mevMessage   :: MessageObject
  }
  deriving (Show, Eq, Generic)

data WebhookPayload = WebhookPayload
  { wpDestination :: Int
  , wpEvents      :: [MessageEvent]
  }
  deriving (Show, Eq, Generic)

--
-- FromJSON Instances
--

instance FromJSON TextMessage where
    parseJSON :: Value -> Parser TextMessage
    parseJSON = withObject "TextMessage" $ \v -> TextMessage
        <$> v .: "id" 
        <*> v .: "text"

instance FromJSON FileInfo where
    parseJSON :: Value -> Parser FileInfo
    parseJSON = withObject "FileInfo" $ \v -> FileInfo
        <$> v .: "name"
        <*> v .: "extension"
        <*> v .: "size"
        <*> v .: "token"

instance FromJSON PreviewImage where
    parseJSON :: Value -> Parser PreviewImage
    parseJSON = withObject "PreviewImage" $ \v -> PreviewImage
        <$> v .: "token"

instance FromJSON ImageMessage where
    parseJSON :: Value -> Parser ImageMessage
    parseJSON = withObject "ImageMessage" $ \v -> ImageMessage
        <$> v .: "previewImage"
        <*> v .: "fileInfo"

instance FromJSON VideoMessage where
    parseJSON :: Value -> Parser VideoMessage
    parseJSON = withObject "VideoMessage" $ \v -> VideoMessage
        <$> v .: "duration"
        <*> v .: "previewImage"
        <*> v .: "fileInfo"

instance FromJSON AudioMessage where
    parseJSON :: Value -> Parser AudioMessage
    parseJSON = withObject "AudioMessage" $ \v -> AudioMessage
        <$> v .: "duration"
        <*> v .: "fileInfo"

instance FromJSON FileMessage where
    parseJSON :: Value -> Parser FileMessage
    parseJSON = withObject "FileMessage" $ \v -> FileMessage
        <$> v .: "fileInfo"

-- | Custom parser for the polymorphic MessageObject.
--   It checks the "type" field to decide how to parse the rest of the object.
instance FromJSON MessageObject where
    parseJSON :: Value -> Parser MessageObject
    parseJSON = withObject "MessageObject" $ \v -> do
        msgType <- v .: "type" :: Parser Text
        case msgType of
            "text"  -> TextMsg  <$> parseJSON (Object v)
            "image" -> ImageMsg <$> parseJSON (Object v)
            "video" -> VideoMsg <$> parseJSON (Object v)
            "audio" -> AudioMsg <$> parseJSON (Object v)
            "file"  -> FileMsg  <$> parseJSON (Object v)
            _       -> fail $ "Unknown message type: " ++ unpack msgType

instance FromJSON EventSource where
    parseJSON :: Value -> Parser EventSource
    parseJSON = withObject "EventSource" $ \v -> EventSource
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

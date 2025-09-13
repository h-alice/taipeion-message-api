-- |
-- Module      : TaipeiOn.Webhook.Event
-- Copyright   : (c) 2025 Wayne Hong
-- License     : BSD-3-Clause
-- Maintainer  : h-alice
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module defines the data types for deserializing incoming webhook events
-- from the TaipeiON platform. These data structures correspond to the JSON
-- objects sent to your webhook URL when a user sends a message to your channel.
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module TaipeiOn.Webhook.Event
    ( -- * Webhook Payload Structure
      WebhookPayload(..)
    , MessageEvent(..)
    , EventSource(..)

      -- * Message Object Types
    , MessageObject(..)
    , TextMessage(..)
    , ImageMessage(..)
    , VideoMessage(..)
    , AudioMessage(..)
    , FileMessage(..)

      -- * File and Media Information
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

-- | Represents a text message received via webhook.
data TextMessage = TextMessage
  { txtId   :: Text -- ^ The unique identifier of the message.
  , txtText :: Text -- ^ The textual content of the message.
  }
  deriving (Show, Eq, Generic)

-- | Contains metadata about a file received in a message.
data FileInfo = FileInfo
  { fiName      :: Text -- ^ The name of the file.
  , fiExtension :: Text -- ^ The file extension (e.g., "jpg", "pdf").
  , fiSize      :: Int  -- ^ The size of the file in bytes.
  , fiToken     :: Text -- ^ The token required to download this file.
  }
  deriving (Show, Eq, Generic)

-- | Contains information for downloading a preview image for media files.
data PreviewImage = PreviewImage
  { piToken :: Text -- ^ The token required to download the preview image.
  }
  deriving (Show, Eq, Generic)

-- | Represents an image message received via webhook.
data ImageMessage = ImageMessage
  { imPreviewImage :: PreviewImage -- ^ Information for the preview image.
  , imFileInfo     :: FileInfo     -- ^ Metadata for the full-sized image file.
  }
  deriving (Show, Eq, Generic)

-- | Represents a video message received via webhook.
data VideoMessage = VideoMessage
  { viDuration     :: Int          -- ^ The duration of the video in milliseconds.
  , viPreviewImage :: PreviewImage -- ^ Information for the preview thumbnail.
  , viFileInfo     :: FileInfo     -- ^ Metadata for the video file.
  }
  deriving (Show, Eq, Generic)

-- | Represents an audio message received via webhook.
data AudioMessage = AudioMessage
  { aiDuration     :: Int      -- ^ The duration of the audio in milliseconds.
  , aiFileInfo     :: FileInfo -- ^ Metadata for the audio file.
  }
  deriving (Show, Eq, Generic)

-- | Represents a generic file message received via webhook.
data FileMessage = FileMessage
  { fmFileInfo :: FileInfo -- ^ Metadata for the file.
  }
  deriving (Show, Eq, Generic)

-- | A sum type that represents any possible message object from a webhook event.
data MessageObject
  = TextMsg  TextMessage
  | ImageMsg ImageMessage
  | VideoMsg VideoMessage
  | AudioMsg AudioMessage
  | FileMsg  FileMessage
  deriving (Show, Eq)

-- | Describes the source of a message event, typically a user.
data EventSource = EventSource
  { esType   :: Text -- ^ The type of the source (e.g., "user").
  , esUserId :: Text -- ^ The unique identifier for the user.
  }
  deriving (Show, Eq, Generic)

-- | Represents a single event within a webhook payload, such as a user
--   sending a message.
data MessageEvent = MessageEvent
  { mevType      :: Text          -- ^ The type of event (e.g., "message").
  , mevTimestamp :: Int           -- ^ The timestamp of the event in seconds since the epoch.
  , mevSource    :: EventSource   -- ^ The source of the event.
  , mevMessage   :: MessageObject -- ^ The message object associated with the event.
  }
  deriving (Show, Eq, Generic)

-- | The top-level data structure for a webhook payload from the TaipeiON platform.
data WebhookPayload = WebhookPayload
  { wpDestination :: Int            -- ^ The channel ID that received the events.
  , wpEvents      :: [MessageEvent] -- ^ A list of events in this payload.
  }
  deriving (Show, Eq, Generic)

--
-- FromJSON Instances for Deserialization
--

instance FromJSON TextMessage where
    parseJSON :: Value -> Parser TextMessage
    parseJSON = withObject "TextMessage" $ \v -> TextMessage
        <$> v .: "id"
        <*> v .: "text"

instance FromJSON FileInfo where
    parseJSON :: Value -> Parser FileInfo
    parseJSON = withObject "FileInfo" $ \v -> FileInfo
        <$> v .: "fileName"
        <*> v .: "fileExtension"
        <*> v .: "fileSize"
        <*> v .: "downloadToken"

instance FromJSON PreviewImage where
    parseJSON :: Value -> Parser PreviewImage
    parseJSON = withObject "PreviewImage" $ \v -> PreviewImage
        <$> v .: "downloadToken"

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

-- | Custom parser for the polymorphic 'MessageObject'.
--   It checks the "type" field in the JSON to decide which specific
--   message constructor to use for parsing.
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

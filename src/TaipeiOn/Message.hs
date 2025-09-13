-- |
-- Module      : TaipeiOn.Message
-- Copyright   : (c) 2025 Wayne Hong
-- License     : BSD-3-Clause
-- Maintainer  : h-alice
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module defines the data types and functions required for constructing
-- the JSON payloads sent to the TaipeiON Message API. It covers various
-- message types like text, image, video, as well as file upload and download
-- requests.
--
-- Tips for development
--
-- To define more payload type, add your field to `ApiPayload`, wrap within a
--`Maybe`, so the field will be omitted while giving an empty value. Add the
-- corresponding field to the `ToJSON` for `ApiPayload`, and `mkEmptyRequest`.
--
-- The final step is adding the maker function (i.e. `mkPrivateMessage`), mind
-- that `apiAsk` field value must match the development manual to make the
-- API calling process work.
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module TaipeiOn.Message
    ( -- * Message Object Types
      TextMessage(..)
    , ImageMessage(..)
    , VideoMessage(..)
    , AudioMessage(..)
    , FileMessage(..)
    , MessageObject(..)

      -- * API Payload Structure
    , ApiPayload(..)

      -- * Message Constructors
    , mkTextMessage
    , mkImageMessage
    , mkVideoMessage
    , mkAudioMessage
    , mkFileMessage

      -- * API Payload Constructors
    , mkBroadcastMessage
    , mkPrivateMessage
    , mkMultiplePrivateMessage
    , mkMessageReadRequest
    , mkFileUpload
    , mkDownloadRequest
    ) where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Base64.Lazy as B64

-- | Represents a text message object.
--   Corresponds to section 2.1 of the API specification.
data TextMessage = TextMessage
  { txtText :: Text -- ^ The content of the text message.
  }
  deriving (Show, Eq, Generic)

-- | Represents an image message object.
--   Corresponds to section 2.2 of the API specification.
data ImageMessage = ImageMessage
  { imgText         :: Maybe Text -- ^ Optional text content captioning the image.
  , imgShowFileName :: Text       -- ^ The file name to be displayed to the user.
  , imgFileID       :: Text       -- ^ The unique identifier for the uploaded image file.
  }
  deriving (Show, Eq, Generic)

-- | Represents a video message object.
--   Corresponds to section 2.3 of the API specification.
data VideoMessage = VideoMessage
  { vidText         :: Maybe Text -- ^ Optional text content captioning the video.
  , vidShowFileName :: Text       -- ^ The file name to be displayed to the user.
  , vidFileID       :: Text       -- ^ The unique identifier for the uploaded video file.
  }
  deriving (Show, Eq, Generic)

-- | Represents an audio message object.
--   Corresponds to section 2.4 of the API specification.
data AudioMessage = AudioMessage
  { audText         :: Maybe Text -- ^ Optional text content captioning the audio.
  , audShowFileName :: Text       -- ^ The file name to be displayed to the user.
  , audFileID       :: Text       -- ^ The unique identifier for the uploaded audio file.
  }
  deriving (Show, Eq, Generic)

-- | Represents a generic file message object.
--   Corresponds to section 2.5 of the API specification.
data FileMessage = FileMessage
  { filText         :: Maybe Text -- ^ Optional text content describing the file.
  , filShowFileName :: Text       -- ^ The file name to be displayed to the user.
  , filFileID       :: Text       -- ^ The unique identifier for the uploaded file.
  }
  deriving (Show, Eq, Generic)

-- | A sum type that represents any possible message object.
--   This allows for polymorphic handling of different message types when
--   constructing API payloads.
data MessageObject
  = TextMsg  TextMessage
  | ImageMsg ImageMessage
  | VideoMsg VideoMessage
  | AudioMsg AudioMessage
  | FileMsg  FileMessage
  deriving (Show, Eq)


-- | Represents the complete JSON payload for any API request. The fields are
--   conditionally included based on the specific action ('apiAsk').
data ApiPayload = ApiPayload
  { apiAsk           :: Text         -- ^ The specific API action to perform (e.g., "broadcastMessage").
  , apiRecipient     :: Maybe Text   -- ^ The recipient's identifier for private messages.
  , apiMessage       :: Maybe MessageObject -- ^ The message object to be sent.
  , apiMessageSN     :: Maybe Int    -- ^ The serial number of a message, used for status checks.
  , apiFileType      :: Maybe Text   -- ^ The file extension (e.g., "jpg") for file uploads.
  , apiFileData      :: Maybe Text   -- ^ The Base64-encoded file data.
  , apiFileName      :: Maybe Text   -- ^ The name of the file to be shown.
  , apiFileIsAsset   :: Maybe Bool   -- ^ Flag indicating if the file is a reusable asset.
  , apiDownloadToken :: Maybe Text   -- ^ The token required to download a file.
  , apiManyRecipient :: Maybe [Text] -- ^ A list of recipients for multi-user private messages.
  }
  deriving (Show, Eq, Generic)

-- | Helper to convert an empty 'Text' to 'Nothing'.
emptyTextMaybe :: Text -> Maybe Text
emptyTextMaybe "" = Nothing
emptyTextMaybe txt = Just txt

-- | Helper function to filter out fields with empty/null values from JSON objects
--   before serialization. This keeps the final JSON payload clean.
notEmpty :: (Key, Value) -> Bool
notEmpty (_, v) = not (isEmpty v)
  where
    isEmpty :: Value -> Bool
    isEmpty (String s) = s == ""
    isEmpty (Array  a) = null a
    isEmpty Null       = True
    isEmpty _          = False

--
-- ToJSON Instances
--

instance ToJSON TextMessage where
    toJSON :: TextMessage -> Value
    toJSON TextMessage{..} =
        object  [ "type" .= ("text" :: Text)
                , "text" .= txtText
                ]

instance ToJSON ImageMessage where
    toJSON :: ImageMessage -> Value
    toJSON ImageMessage{..} =
        object $ filter notEmpty
            [ "type"         .= ("image" :: Text)
            , "text"         .= imgText
            , "showFileName" .= imgShowFileName
            , "fileID"       .= imgFileID
            ]

instance ToJSON VideoMessage where
    toJSON :: VideoMessage -> Value
    toJSON VideoMessage{..} =
        object $ filter notEmpty
            [ "type"         .= ("video" :: Text)
            , "text"         .= vidText
            , "showFileName" .= vidShowFileName
            , "fileID"       .= vidFileID
            ]

instance ToJSON AudioMessage where
    toJSON :: AudioMessage -> Value
    toJSON AudioMessage{..} =
        object $ filter notEmpty
            [ "type"         .= ("audio" :: Text)
            , "text"         .= audText
            , "showFileName" .= audShowFileName
            , "fileID"       .= audFileID
            ]

instance ToJSON FileMessage where
    toJSON :: FileMessage -> Value
    toJSON FileMessage{..} =
        object $ filter notEmpty
            [ "type"         .= ("file" :: Text)
            , "text"         .= filText
            , "showFileName" .= filShowFileName
            , "fileID"       .= filFileID
            ]

-- | The ToJSON instance for the polymorphic wrapper.
--   Delegates serialization to the specific message type's instance.
instance ToJSON MessageObject where
    toJSON :: MessageObject -> Value
    toJSON (TextMsg  msg)  = toJSON msg
    toJSON (ImageMsg msg)  = toJSON msg
    toJSON (VideoMsg msg)  = toJSON msg
    toJSON (AudioMsg msg)  = toJSON msg
    toJSON (FileMsg  msg)  = toJSON msg

-- | Custom ToJSON instance for 'ApiPayload' which filters out 'Nothing' fields.
instance ToJSON ApiPayload where
    toJSON :: ApiPayload -> Value
    toJSON ApiPayload{..} =
        object $ filter notEmpty
            [ "ask"            .= apiAsk
            , "recipient"      .= apiRecipient
            , "message"        .= apiMessage
            , "messageSN"      .= apiMessageSN
            , "file_type"      .= apiFileType
            , "data_binary"    .= apiFileData
            , "showName"       .= apiFileName
            , "isAsset"        .= apiFileIsAsset
            , "download_token" .= apiDownloadToken
            , "recipientList"  .= apiManyRecipient
            ]

-- | Constructs a 'TextMessage'.
mkTextMessage
  :: Text   -- ^ The message text content.
  -> MessageObject
mkTextMessage msg = TextMsg (TextMessage { txtText = msg })

-- | Constructs an 'ImageMessage'.
mkImageMessage
  :: Maybe Text   -- ^ An optional text caption.
  -> Text         -- ^ The display file name.
  -> Text         -- ^ The image file ID obtained from uploading.
  -> MessageObject
mkImageMessage optMsg fileName fileID
  = ImageMsg $ ImageMessage { imgText = optMsg
                            , imgShowFileName = fileName
                            , imgFileID = fileID
                            }

-- | Constructs a 'VideoMessage'.
mkVideoMessage
  :: Maybe Text -- ^ An optional text caption.
  -> Text       -- ^ The display file name.
  -> Text       -- ^ The video file ID obtained from uploading.
  -> MessageObject
mkVideoMessage optMsg fileName fileID
  = VideoMsg $ VideoMessage { vidText = optMsg
                            , vidShowFileName = fileName
                            , vidFileID = fileID
                            }

-- | Constructs an 'AudioMessage'.
mkAudioMessage
  :: Maybe Text   -- ^ An optional text caption.
  -> Text         -- ^ The display file name.
  -> Text         -- ^ The audio file ID obtained from uploading.
  -> MessageObject
mkAudioMessage optMsg fileName fileID
  = AudioMsg $ AudioMessage { audText = optMsg
                            , audShowFileName = fileName
                            , audFileID = fileID
                            }

-- | Constructs a 'FileMessage'.
mkFileMessage
  :: Text         -- ^ An optional text description.
  -> Text         -- ^ The display file name.
  -> Text         -- ^ The file ID obtained from uploading.
  -> MessageObject
mkFileMessage optMsg fileName fileID
  = FileMsg $ FileMessage { filText = emptyTextMaybe optMsg
                          , filShowFileName = fileName
                          , filFileID = fileID
                          }

-- | Internal helper to create a blank 'ApiPayload'.
mkEmptyRequest :: ApiPayload
mkEmptyRequest = ApiPayload
                        { apiAsk = ""
                        , apiRecipient     = Nothing
                        , apiMessage       = Nothing
                        , apiMessageSN     = Nothing
                        , apiFileType      = Nothing
                        , apiFileData      = Nothing
                        , apiFileName      = Nothing
                        , apiFileIsAsset   = Nothing
                        , apiDownloadToken = Nothing
                        , apiManyRecipient = Nothing
                        }

-- | Constructs a payload for broadcasting a message to all users in a channel.
mkBroadcastMessage
    :: MessageObject -- ^ The message to send.
    -> ApiPayload
mkBroadcastMessage msg = mkEmptyRequest { apiAsk = "broadcastMessage"
                                        , apiMessage = Just msg
                                        }

-- | Constructs a payload for sending a private message to a single recipient.
mkPrivateMessage
    :: Text          -- ^ The recipient's user identifier.
    -> MessageObject -- ^ The message to send.
    -> ApiPayload
mkPrivateMessage recipient msg = mkEmptyRequest { apiAsk = "sendMessage"
                                                , apiRecipient    = Just recipient
                                                , apiMessage      = Just msg
                                                }

-- | Constructs a payload for sending a private message to multiple recipients.
mkMultiplePrivateMessage
    :: [Text]        -- ^ A list of recipient user identifiers.
    -> MessageObject -- ^ The message to send.
    -> ApiPayload
mkMultiplePrivateMessage recipient msg = mkEmptyRequest
                                              { apiAsk = "broadcastMessageByLoginNameList"
                                              , apiManyRecipient = Just recipient
                                              , apiMessage       = Just msg
                                              }

-- | Constructs a payload to request the read status of a specific message.
mkMessageReadRequest
    :: Int -- ^ The serial number (`MessageSN`) of the message to check.
    -> ApiPayload
mkMessageReadRequest msgSN = mkEmptyRequest { apiAsk = "getMsgReadStatus"
                                            , apiMessageSN    = Just msgSN
                                            }

-- | Constructs a payload for uploading a file.
mkFileUpload
    :: Text             -- ^ The file name to display.
    -> Text             -- ^ The file extension (e.g., "jpg", "pdf").
    -> BL.ByteString    -- ^ The raw binary data of the file.
    -> Maybe Bool       -- ^ 'Just True' if the file should be a reusable asset.
    -> ApiPayload
mkFileUpload fileName fileExt fileData fileIsAsset = mkEmptyRequest
                                          { apiAsk = "uploadFile"
                                          , apiFileType     = Just fileExt
                                          , apiFileData     = Just  $ TL.toStrict
                                                                    $ TL.decodeLatin1
                                                                    $ B64.encode fileData
                                          , apiFileName     = emptyTextMaybe fileName
                                          , apiFileIsAsset  = fileIsAsset
                                          }

-- | Constructs a payload to download a file using a token.
mkDownloadRequest
    :: Text -- ^ The download token received from a webhook event.
    -> ApiPayload
mkDownloadRequest downloadToken = mkEmptyRequest
                                    { apiAsk = "downloadFile"
                                    , apiDownloadToken = Just downloadToken
                                    }

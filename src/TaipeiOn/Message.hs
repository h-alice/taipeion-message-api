-- Message API payload crafting
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module TaipeiOn.Message
    ( TextMessage(..)
    , ImageMessage(..)
    , VideoMessage(..)
    , AudioMessage(..)
    , FileMessage(..)
    , MessageObject(..)
    , ApiPayload(..)
    , mkTextMessage
    , mkImageMessage
    , mkVideoMessage
    , mkAudioMessage
    , mkFileMessage
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
  { txtText :: Text
  }
  deriving (Show, Eq, Generic)

-- | Represents an image message object.
--   Corresponds to section 2.2 of the API specification.
data ImageMessage = ImageMessage
  { imgText         :: Maybe Text -- ^ Optional text content
  , imgShowFileName :: Text       -- ^ File name
  , imgFileID       :: Text       -- ^ File ID
  }
  deriving (Show, Eq, Generic)

-- | Represents a video message object.
--   Corresponds to section 2.3 of the API specification.
data VideoMessage = VideoMessage
  { vidText         :: Maybe Text -- Optional text content
  , vidShowFileName :: Text
  , vidFileID       :: Text
  }
  deriving (Show, Eq, Generic)

-- | Represents an audio message object.
--   Corresponds to section 2.4 of the API specification.
data AudioMessage = AudioMessage
  { audText         :: Maybe Text -- Optional text content
  , audShowFileName :: Text
  , audFileID       :: Text
  }
  deriving (Show, Eq, Generic)

-- | Represents a generic file message object.
--   Corresponds to section 2.5 of the API specification.
data FileMessage = FileMessage
  { filText         :: Maybe Text -- Optional text content
  , filShowFileName :: Text
  , filFileID       :: Text
  }
  deriving (Show, Eq, Generic)

-- | A sum type that represents any possible message object.
--   This allows for polymorphic handling of different message types.
data MessageObject
  = TextMsg  TextMessage
  | ImageMsg ImageMessage
  | VideoMsg VideoMessage
  | AudioMsg AudioMessage
  | FileMsg  FileMessage
  deriving (Show, Eq)


-- Main API payload structure
data ApiPayload = ApiPayload
  { apiAsk           :: Text
  , apiRecipient     :: Maybe Text
  , apiMessage       :: Maybe MessageObject
  , apiMessageSN     :: Maybe Int
  , apiFileType      :: Maybe Text
  , apiFileData      :: Maybe Text
  , apiFileName      :: Maybe Text
  , apiFileIsAsset   :: Maybe Bool
  , apiDownloadToken :: Maybe Text
  , apiManyRecipient :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

emptyTextMaybe :: Text -> Maybe Text
emptyTextMaybe "" = Nothing
emptyTextMaybe txt = Just txt



--

-- Helper function to filter out fields with empty/null values from JSON objects.
-- This style is adopted from the provided Message.hs file.
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
--
-- Arguments:
--
-- * @Text@:       The message text
mkTextMessage 
  :: Text   -- ^ The message
  -> MessageObject
mkTextMessage msg = TextMsg (TextMessage { txtText = msg })

-- | Constructs an 'ImageMessage'.
--
-- Arguments:
--
-- * @Maybe Text@: An optional text message (unsupported in API v1.6)
-- * @Text@:       The display file name
-- * @Text@:       The image file ID
mkImageMessage
  :: Maybe Text  
  -> Text        
  -> Text       
  -> MessageObject
mkImageMessage optMsg fileName fileID 
  = ImageMsg $ ImageMessage { imgText = optMsg
                            , imgShowFileName = fileName
                            , imgFileID = fileID
                            }

-- | Constructs a 'VideoMessage'.
--
-- Arguments:
--
-- * @Maybe Text@:  An optional text message (unsupported in API v1.6)
-- * @Text@:        The display file name
-- * @Text@:        The video file ID
mkVideoMessage 
  :: Maybe Text -- ^ Optional text message
  -> Text       -- ^ File name
  -> Text       -- ^ File ID
  -> MessageObject
mkVideoMessage optMsg fileName fileID 
  = VideoMsg $ VideoMessage { vidText = optMsg
                            , vidShowFileName = fileName
                            , vidFileID = fileID
                            }

-- | Constructs an 'AudioMessage'.
--
-- Arguments:
--
-- * @Maybe Text@:  An optional text message (unsupported in API v1.6)
-- * @Text@:        The display file name
-- * @Text@:        The audio file ID
mkAudioMessage 
  :: Maybe Text 
  -> Text 
  -> Text 
  -> MessageObject
mkAudioMessage optMsg fileName fileID 
  = AudioMsg $ AudioMessage { audText = optMsg
                            , audShowFileName = fileName
                            , audFileID = fileID
                            }

-- | Constructs a 'FileMessage'.
--
-- Arguments:
--
-- * @Maybe Text@:  An optional text message (unsupported in API v1.6)
-- * @Text@:        The display file name
-- * @Text@:        The file ID
mkFileMessage 
  :: Text 
  -> Text 
  -> Text 
  -> MessageObject
mkFileMessage optMsg fileName fileID 
  = FileMsg $ FileMessage { filText = emptyTextMaybe optMsg
                          , filShowFileName = fileName
                          , filFileID = fileID
                          }



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
-- | Constructs a 'BroadcastMessage'
--
-- Arguments:
--
-- * @MessageObject@:   Message to send
mkBroadcastMessage :: MessageObject -> ApiPayload
mkBroadcastMessage msg = mkEmptyRequest { apiAsk = "broadcastMessage"
                                        , apiMessage = Just msg 
                                        }



-- | Constructs a 'PrivateMessage'
--
-- Arguments:
--
-- * @Text@:            The message recipient
-- * @MessageObject@:   Message to send
mkPrivateMessage :: Text -> MessageObject -> ApiPayload
mkPrivateMessage recipient msg = mkEmptyRequest { apiAsk = "sendMessage"
                                                , apiRecipient    = Just recipient
                                                , apiMessage      = Just msg 
                                                }

mkMultiplePrivateMessage :: [Text] -> MessageObject -> ApiPayload
mkMultiplePrivateMessage recipient msg = mkEmptyRequest 
                                              { apiAsk = "broadcastMessageByLoginNameList"
                                              , apiManyRecipient = Just recipient
                                              , apiMessage       = Just msg 
                                              }

mkMessageReadRequest :: Int -> ApiPayload
mkMessageReadRequest msgSN = mkEmptyRequest { apiAsk = "getMsgReadStatus"
                                            , apiMessageSN    = Just msgSN
                                            }


mkFileUpload :: Text -> Text -> BL.ByteString -> Maybe Bool -> ApiPayload
mkFileUpload fileName fileExt fileData fileIsAsset = mkEmptyRequest
                                          { apiAsk = "uploadFile"
                                          , apiFileType     = Just fileExt
                                          , apiFileData     = Just  $ TL.toStrict 
                                                                    $ TL.decodeLatin1 
                                                                    $ B64.encode fileData
                                          , apiFileName     = emptyTextMaybe fileName
                                          , apiFileIsAsset  = fileIsAsset
                                          }

mkDownloadRequest :: Text -> ApiPayload
mkDownloadRequest downloadToken = mkEmptyRequest  
                                    { apiAsk = "downloadFile"
                                    , apiDownloadToken = Just downloadToken
                                    }


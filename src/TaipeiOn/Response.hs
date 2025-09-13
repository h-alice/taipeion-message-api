-- |
-- Module      : TaipeiOn.Response
-- Copyright   : (c) 2025 Wayne Hong
-- License     : BSD-3-Clause
-- Maintainer  : h-alice
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module defines the data types for deserializing responses from the
-- TaipeiON API. It also provides helper functions to automatically parse
-- raw HTTP responses into the appropriate structured data types, including
-- handling of error responses.
--
-- Tips for development
--
-- If you need to defining more response type, you will need to define the
-- structure and `FromJSON` instance. Then you'll need to add your response
-- type to the sum type `TpoResponse`.
-- 
-- You may also want to make your deserialization method, so `decodeTpoResponse`
-- can work properly.
--

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TaipeiOn.Response
    ( -- * Specific Response Types
      ResponseSendMessage(..)
    , ResponseUploadFile(..)
    , ResponseReadCount(..)
    , ResponseError(..)
    , ReadDetail(..)

      -- * General Response Wrapper
    , TpoResponse(..)

      -- * Response Decoders
    , decodeGeneralResponse
    , decodeMessageResponse
    , decodeUploadFileResponse
    , decodeReadCountResponse

    ) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Network.HTTP.Client as H
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode)
import Data.ByteString (toStrict)

--
-- Response type and structure
--

-- | Represents the successful response from sending a message.
data ResponseSendMessage = ResponseSendMessage
  { resMessageSN :: Int -- ^ The unique serial number of the sent message.
  } deriving (Show, Eq)

-- | Represents the successful response from uploading a file.
data ResponseUploadFile = ResponseUploadFile
  { resFileID :: String -- ^ The unique identifier for the uploaded file.
  } deriving (Show, Eq)

-- | Represents a general, unparsed HTTP response. This is used as a fallback
--   when a specific JSON structure is not expected or parsing fails.
data ResponseGeneral = ResponseGeneral
  { resStatusCode :: Int         -- ^ The HTTP status code of the response.
  , resContent    :: BS.ByteString -- ^ The raw response body.
  } deriving (Show, Eq)

-- | Represents an error response from the API.
data ResponseError = ResponseError
  { resErrorMessage :: Text -- ^ A summary of the error.
  , resErrorDetails :: Text -- ^ Detailed information about the error.
  } deriving (Show, Eq)

-- | Contains details about a single user who has read a message.
data ReadDetail = ReadDetail
  { resReadAccountID :: String -- ^ The account ID of the user who read the message.
  , resReadTimestamp :: String -- ^ The timestamp when the message was read.
  } deriving (Show, Eq)

-- | Represents the response for a message read status query.
data ResponseReadCount = ResponseReadCount
              { resReadCount   :: Int          -- ^ The total number of users who have read the message.
              , resReadDetails :: [ReadDetail] -- ^ A list of details for each user who has read the message.
              } deriving (Show, Eq)

-- | A sum type that encapsulates all possible structured responses from the API.
--   This allows client functions to return a single, consistent type.
data TpoResponse
  = SendMessage             ResponseSendMessage
  | UploadFileResponse      ResponseUploadFile
  | ReadCount               ResponseReadCount
  | ErrorResponse           ResponseError
  | General                 ResponseGeneral
  deriving (Show, Eq)

--
-- FromJSON Instances
--

instance FromJSON ResponseSendMessage where
  parseJSON :: Value -> Parser ResponseSendMessage
  parseJSON = withObject "ResponseSendMessage" $ \v ->
    ResponseSendMessage <$> v .: "MessageSN"

instance FromJSON ResponseUploadFile where
  parseJSON :: Value -> Parser ResponseUploadFile
  parseJSON = withObject "ResponseUploadFile" $ \v ->
    ResponseUploadFile <$> v .: "FileID"

instance FromJSON ResponseError where
  parseJSON :: Value -> Parser ResponseError
  parseJSON = withObject "ResponseError" $ \v ->
    ResponseError <$> v .: "message"
                  <*> v .: "details"

instance FromJSON ReadDetail where
  parseJSON :: Value -> Parser ReadDetail
  parseJSON = withObject "ReadDetail" $ \v ->
    ReadDetail  <$> v .: "Account"
                <*> v .: "ReadTime"

instance FromJSON ResponseReadCount where
  parseJSON :: Value -> Parser ResponseReadCount
  parseJSON = withObject "ResponseReadCount" $ \v ->
    ResponseReadCount <$> v .: "ReadCount"
                      <*> v .: "ReadDetailList"

-- | Decodes a raw HTTP response into a 'TpoResponse.General' wrapper. This is the
--   ultimate fallback if no other decoding succeeds.
decodeGeneralResponse :: H.Response LBS.ByteString -> TpoResponse
decodeGeneralResponse resp = General ResponseGeneral
                                { resStatusCode = getResponseStatusCode resp
                                , resContent = toStrict $ getResponseBody resp
                                }

-- | A higher-order function to decode an HTTP response. It first tries to
--   decode the body into the specified 'a' type. If that fails, it tries to decode
--   it as a 'ResponseError'. If both fail, it wraps the raw response in
--   'TpoResponse.General'.
decodeTpoResponse :: (FromJSON a) => (a -> TpoResponse) -> H.Response LBS.ByteString -> TpoResponse
decodeTpoResponse constructor resp =
  let body = getResponseBody resp
  in case eitherDecode body of
    Right r -> constructor r
    Left _ ->
      case eitherDecode body of
        Right r -> ErrorResponse r
        Left _ -> decodeGeneralResponse resp

-- | Decodes an HTTP response expected to contain a 'ResponseSendMessage'.
decodeMessageResponse :: H.Response LBS.ByteString -> TpoResponse
decodeMessageResponse = decodeTpoResponse SendMessage

-- | Decodes an HTTP response expected to contain a 'ResponseUploadFile'.
decodeUploadFileResponse :: H.Response LBS.ByteString -> TpoResponse
decodeUploadFileResponse = decodeTpoResponse UploadFileResponse

-- | Decodes an HTTP response expected to contain a 'ResponseReadCount'.
decodeReadCountResponse :: H.Response LBS.ByteString -> TpoResponse
decodeReadCountResponse = decodeTpoResponse ReadCount

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TaipeiOn.Response
    ( ResponseSendMessage(..)
    , ResponseUploadFile(..)
    , ResponseReadCount(..)
    , ResponseError(..)
    , ReadDetail(..)
    , TpoResponse(..)
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

-- | Sub-data structures

data ResponseSendMessage = ResponseSendMessage
  { resMessageSN :: Int
  } deriving (Show, Eq)

data ResponseUploadFile = ResponseUploadFile
  { resFileID :: Int
  } deriving (Show, Eq)

data ResponseGeneral = ResponseGeneral
  { resStatusCode :: Int
  , resContent    :: BS.ByteString
  } deriving (Show, Eq)

data ResponseError = ResponseError
  { resErrorMessage :: Text
  , resErrorDetails :: Text
  } deriving (Show, Eq)

data ReadDetail = ReadDetail
  { resReadAccountID :: String
  , resReadTimestamp :: String
  } deriving (Show, Eq)

data ResponseReadCount = ResponseReadCount
              { resReadCount   :: Int
              , resReadDetails :: [ReadDetail]
              } deriving (Show, Eq)

-- | Sum type
data TpoResponse
  = SendMessage     ResponseSendMessage
  | UploadFile      ResponseUploadFile
  | ReadCount       ResponseReadCount
  | ErrorResponse   ResponseError
  | General         ResponseGeneral
  deriving (Show, Eq)

-- Sub-types JSON deserializers

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

decodeGeneralResponse :: H.Response LBS.ByteString -> TpoResponse
decodeGeneralResponse resp = General ResponseGeneral 
                                { resStatusCode = getResponseStatusCode resp
                                , resContent = toStrict $ getResponseBody resp
                                }

decodeMessageResponse :: H.Response LBS.ByteString -> TpoResponse
decodeMessageResponse resp =
  case ( eitherDecode (getResponseBody resp) :: Either String ResponseSendMessage ) of
    Right r -> SendMessage r
    Left _ -> 
      case ( eitherDecode (getResponseBody resp) :: Either String ResponseError ) of
        Right r -> ErrorResponse r
        Left _ -> decodeGeneralResponse resp

decodeUploadFileResponse :: H.Response LBS.ByteString -> TpoResponse
decodeUploadFileResponse resp =
  case ( eitherDecode (getResponseBody resp) :: Either String ResponseUploadFile ) of
    Right r -> UploadFile r
    Left _ -> 
      case ( eitherDecode (getResponseBody resp) :: Either String ResponseError ) of
        Right r -> ErrorResponse r
        Left _ -> decodeGeneralResponse resp

decodeReadCountResponse :: H.Response LBS.ByteString -> TpoResponse
decodeReadCountResponse resp =
  case ( eitherDecode (getResponseBody resp) :: Either String ResponseReadCount ) of
    Right r -> ReadCount r
    Left _ -> 
      case ( eitherDecode (getResponseBody resp) :: Either String ResponseError ) of
        Right r -> ErrorResponse r
        Left _ -> decodeGeneralResponse resp
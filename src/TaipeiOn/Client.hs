-- |
-- Module      : TaipeiOn.Client
-- Copyright   : (c) 2025 Wayne Hong
-- License     : BSD-3-Clause
-- Maintainer  : h-alice
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides the main client for interacting with the TaipeiON Message
-- API. It defines the necessary data structures for configuration ('Channel')
-- and for specifying API calls ('Action'), and exposes a single function,
-- 'tpoClient', to execute requests.
--
-- Tips for developmenting
--
-- If you want to add more client actions, define the actio in `data Action` 
-- section first. Then add the corresponding request generating strategy 
-- at `mkTpoRequest`. And finally, update the `tpoClient`.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module TaipeiOn.Client
    ( -- * Client Configuration and Actions
      Action(..)
    , Channel(..)
      -- * Main Client Function
    , tpoClient
    ) where

import TaipeiOn.Response
import TaipeiOn.Message
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Aeson as AE (encode)
import qualified Data.Text.Encoding as TE
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Conduit
    ( tlsManagerSettings,
      parseRequest,
      Request(requestBody, method, requestHeaders),
      newManager,
      RequestBody(RequestBodyLBS),
      httpLbs )
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Client.Conduit (Request(redactHeaders))

-- | Describes a channel, containing all necessary credentials for API access.
data Channel = Channel
  { chanId        :: Int    -- ^ The unique numeric ID of the channel.
  , chanToken     :: Text   -- ^ The channel's access token for authorization.
  , chanSecret    :: Text   -- ^ The channel's secret token (for sender verification).
  , chanApiToken  :: Text   -- ^ The API platform subscription key.
  }
  deriving (Show, Eq, Generic)

-- | Represents possible actions that can be performed with the client.
--   Each constructor holds the necessary information for a specific API call.
data Action
  = WriteChannelBroadcast Channel MessageObject
    -- ^ Sends a message to all users in the channel.
  | WriteChannelPrivate Channel Text MessageObject
    -- ^ Sends a private message to a single user. The 'Text' is the recipient's ID.
  | WriteChannelMultiplePrivate Channel [Text] MessageObject
    -- ^ Sends a private message to a list of users.
  | GetMessageReadStatus Channel Int
    -- ^ Retrieves the read status of a message. The 'Int' is the message's serial number.
  | UploadFile Channel Text Text BL.ByteString (Maybe Bool)
    -- ^ Uploads a file. Parameters are: Channel, FileName, FileExtension, FileData, IsAsset.
  | DownloadFile Channel Text
    -- ^ Downloads a file. The 'Text' is the download token from a webhook event.
  deriving (Show, Eq, Generic)


-- | (Internal) Creates the necessary HTTP headers for a request.
mkTpoHeader :: Channel -> [(HeaderName, ByteString)]
mkTpoHeader chan =
    [ ("Content-Type", "application/json; charset=utf-8")
    , ("User-Agent", "TaipeiON Message Client v0.1")
    , ("Authorization", TE.encodeUtf8 (chanToken chan))
    , ("Ocp-Apim-Subscription-Key", TE.encodeUtf8 (chanApiToken chan))
    ]

-- | (Internal) Applies the standard channel headers to an HTTP request.
setTpoChannelHeader :: Channel -> Request -> Request
setTpoChannelHeader chan oldReq = oldReq { requestHeaders = mkTpoHeader chan }

-- | (Internal) Constructs the full HTTP 'Request' based on the 'Action'.
--   This includes setting the correct headers, method, and request body.
mkTpoRequest :: Action -> Request -> Request
mkTpoRequest action req =
  case action of
    WriteChannelBroadcast chan msg ->
      setTpoChannelHeader chan req
        { requestBody = RequestBodyLBS $ AE.encode $ mkBroadcastMessage msg
        }
    WriteChannelPrivate chan recipient msg ->
      setTpoChannelHeader chan req
        {requestBody = RequestBodyLBS $ AE.encode $ mkPrivateMessage recipient msg
        }
    WriteChannelMultiplePrivate chan recipients msg ->
      setTpoChannelHeader chan req
        {requestBody = RequestBodyLBS $ AE.encode $ mkMultiplePrivateMessage recipients msg
        }
    GetMessageReadStatus chan msgSN ->
      setTpoChannelHeader chan req
        {requestBody = RequestBodyLBS $ AE.encode $ mkMessageReadRequest msgSN
        }
    UploadFile chan fileName fileExt fileData fileIsAsset ->
      setTpoChannelHeader chan req
        { requestBody = RequestBodyLBS  $ AE.encode
                                        $ mkFileUpload fileName fileExt fileData fileIsAsset
        }
    DownloadFile chan downloadToken ->
      setTpoChannelHeader chan req
        { requestBody = RequestBodyLBS  $ AE.encode
                                        $ mkDownloadRequest downloadToken
        }

-- | The main client function for interacting with the TaipeiON API.
--
--   This function takes an API endpoint URL and an 'Action', performs the
--   corresponding HTTP request, and returns a 'TpoResponse' representing
--   the parsed result.
--
--   Example:
--   > let channel = Channel 123 "token" "secret" "api-key"
--   > let message = mkTextMessage "Hello, world!"
--   > let action = WriteChannelBroadcast channel message
--   > response <- tpoClient "https://api.endpoint.url/v1" action
--
tpoClient :: String -> Action -> IO TpoResponse
tpoClient endPoint action = do

  -- Setup HTTP connection manager
  manager <- newManager tlsManagerSettings

  -- Initialize request from the base endpoint URL
  vanillaReq <- parseRequest endPoint

  -- Create the full, configured request
  let req = mkTpoRequest action vanillaReq
                                  { method = "POST"
                                  -- Redact sensitive headers from logs
                                  , redactHeaders
                                      = Set.fromList
                                          [ "Authorization"
                                          , "Ocp-Apim-Subscription-Key"
                                          ]
                                  }

  -- Send the request and receive the response
  resp <- httpLbs req manager

  -- Decode the response based on the action that was performed
  case action of
    WriteChannelBroadcast {} -> pure $ decodeMessageResponse resp
    WriteChannelPrivate {}   -> pure $ decodeMessageResponse resp
    WriteChannelMultiplePrivate {} -> pure $ decodeMessageResponse resp
    GetMessageReadStatus {}  -> pure $ decodeReadCountResponse resp
    UploadFile {}  -> pure $ decodeUploadFileResponse resp
    DownloadFile {}  -> pure $ decodeGeneralResponse resp

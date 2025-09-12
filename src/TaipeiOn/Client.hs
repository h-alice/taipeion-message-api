{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

-- src/TaipeiOn/Webhook/Event.hs
module TaipeiOn.Client
    ( Action(..)
    , Channel(..)
    , tpoClient
    ) where

import TaipeiOn.Response 

import qualified Data.Aeson as AE ( encode )
import Network.HTTP.Conduit
    ( tlsManagerSettings,
      parseRequest,
      Request(requestBody, method, requestHeaders),
      newManager,
      RequestBody(RequestBodyLBS),
      httpLbs )
import TaipeiOn.Message
import GHC.Generics ( Generic )
import Data.Text ( Text )
import qualified Data.Text.Encoding as TE
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL

import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Client.Conduit (Request(redactHeaders))


-- | Describes a channel.
--
-- Arguments:
--
-- * @Int@:     The unique ID of a channel
-- * @Text@:    The access token of channel
-- * @Text@:    The secret token of channel
-- * @Text@:    The API platform token of service
data Channel = Channel
  { chanId        :: Int
  , chanToken     :: Text
  , chanSecret    :: Text
  , chanApiToken  :: Text
  }
  deriving (Show, Eq, Generic)

-- | Client actions
-- 
-- Actions:
--
-- * @WriteChannelBroadcast@        Write broadcast message to channel.
-- * @WriteChannelPrivate@          Write private message to channel.
-- * @WriteChannelMultiplePrivate@  Write private message to channel, with multiple recipients.
-- * @GetMessageReadStatus@         Get message read status.
-- * @UploadFile@                   Upload file to channel.
-- * @DownloadFile@                 Download file from channel.
data Action 
  = WriteChannelBroadcast Channel MessageObject
  | WriteChannelPrivate Channel Text MessageObject
  | WriteChannelMultiplePrivate Channel [Text] MessageObject
  | GetMessageReadStatus Channel Int
  | UploadFile Channel Text Text BL.ByteString (Maybe Bool)
  | DownloadFile Channel Text
  deriving (Show, Eq, Generic)


-- Make channel-related headers. Internal function.
mkTpoHeader :: Channel -> [(HeaderName, ByteString)]
mkTpoHeader chan = 
    [ ("Content-Type", "application/json; charset=utf-8")
    , ("User-Agent", "TaipeiON Message Client v0.1")
    , ("Authorization", TE.encodeUtf8 (chanToken chan))
    , ("Ocp-Apim-Subscription-Key", TE.encodeUtf8 (chanApiToken chan))
    ]

-- Set channel-related headers. Internal function.
setTpoChannelHeader :: Channel -> Request -> Request 
setTpoChannelHeader chan oldReq = oldReq { requestHeaders = mkTpoHeader chan }

-- Make TaipeiON request, edit if need to implement more action.
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

-- Main client function
tpoClient :: String -> Action -> IO TpoResponse
tpoClient endPoint action = do
  
  -- Setup HTTP connection manager
  manager <- newManager tlsManagerSettings

  -- Initialize request
  vanillaReq <- parseRequest endPoint

  -- Create actual request
  let req = mkTpoRequest action vanillaReq 
                                  { method = "POST"
                                  , redactHeaders 
                                      = Set.fromList 
                                          [ "Authorization"
                                          , "Ocp-Apim-Subscription-Key"
                                          ]
                                  } 

  -- Send request
  resp <- httpLbs req manager

  case action of
    WriteChannelBroadcast {} -> pure $ decodeMessageResponse resp
    WriteChannelPrivate {}   -> pure $ decodeMessageResponse resp
    WriteChannelMultiplePrivate {} -> pure $ decodeMessageResponse resp
    GetMessageReadStatus {}  -> pure $ decodeReadCountResponse resp
    UploadFile {}  -> pure $ decodeUploadFileResponse resp
    DownloadFile {}  -> pure $ decodeGeneralResponse resp
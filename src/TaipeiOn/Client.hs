{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}



-- src/TaipeiOn/Webhook/Event.hs
module TaipeiOn.Client
    ( Action(..)
    , Channel(..)
    , tpoClient
    ) where

import qualified Data.Aeson as AE ( encode )
import Network.HTTP.Conduit
import TaipeiOn.Message
import GHC.Generics
import Data.Text
import qualified Data.Text.Encoding as TE
import qualified Data.Set as Set
import Data.ByteString (ByteString)

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

data Action 
  = WriteChannelBroadcast Channel MessageObject
  | WriteChannelPrivate Channel Text MessageObject

mkTpoHeader :: Channel -> [(HeaderName, ByteString)]
mkTpoHeader chan = 
    [ ("Content-Type", "application/json; charset=utf-8")
    , ("User-Agent", "TaipeiON Message Client v0.1")
    , ("Authorization", TE.encodeUtf8 (chanToken chan))
    , ("Ocp-Apim-Subscription-Key", TE.encodeUtf8 (chanApiToken chan))
    ]

mkTpoRequest :: Request -> Action -> Request
mkTpoRequest req action =
  case action of
    WriteChannelBroadcast chan msg -> 
      req 
        { method = "POST"
        , requestHeaders = mkTpoHeader chan
        , requestBody = RequestBodyLBS $ AE.encode $ mkBroadcastMessage msg
        , redactHeaders = Set.fromList ["Authorization", "Ocp-Apim-Subscription-Key"]
        }
    WriteChannelPrivate chan recipient msg -> 
      req 
        { method = "POST"
        , requestHeaders = mkTpoHeader chan
        , requestBody = RequestBodyLBS $ AE.encode $ mkPrivateMessage recipient msg
        , redactHeaders = Set.fromList ["Authorization", "Ocp-Apim-Subscription-Key"]
        }

tpoClient :: String -> Action -> IO(Either String String)
tpoClient endPoint action = do
  
  manager <- newManager tlsManagerSettings

  vanillaReq <- parseRequest endPoint

  putStrLn "--- Vanilla Request ---"
  putStrLn $ show vanillaReq


  let req = mkTpoRequest vanillaReq action
  
  putStrLn "--- Final Request ---"
  putStrLn $ show req

  putStrLn "--- Sending Request ---"
  resp <- httpLbs req manager

  putStrLn $ show resp

  pure $ Right ""
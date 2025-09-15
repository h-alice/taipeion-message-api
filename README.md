# taipeion-api
A Haskell client library for interacting with the TaipeiON Message API. 

This library provides a strongly-typed interface for sending messages, uploading/downloading files, and parser for incoming webhook events, with clean functional pattern and user friendly design.

## Usage

### Message API

#### Client Initialization

To inteact with TaipeiON API, first define your channel credentials and the API endpoint.

```haskell
import TaipeiOn.Client
import TaipeiOn.Message
import TaipeiOn.Response
import Data.Text (Text)

-- Replace with your channel credentials
myChannel :: Channel
myChannel = Channel
  { chanId = 12345
  , chanToken = "CHANNEL_ACCESS_TOKEN"
  , chanSecret = "CHANNEL_SECRET"
  , chanApiToken = "API_PLATFORM_SUBSCRIPTION_KEY"
  }

-- The TaipeiON API endpoint
apiEndpoint :: String
apiEndpoint = "https://myapi.gov.taipei/m-taipeion/MessageFeedService"
```

### Sending a Message

To send a message, construct a MessageObject, wrap it in an Action, and pass it to the tpoClient.

```haskell
-- Example: Sending a broadcast text message
sendGreeting :: IO ()
sendGreeting = do
  let message = mkTextMessage "Hello from the Haskell client!"
  let action = WriteChannelBroadcast myChannel message

  putStrLn "Sending broadcast message..."
  response <- tpoClient apiEndpoint action

  -- Handle the response
  case response of
    SendMessage smResp ->
      putStrLn $ "Message sent successfully! MessageSN: " ++ show (resMessageSN smResp)
    ErrorResponse errResp ->
      putStrLn $ "API Error: " ++ show (resErrorMessage errResp)
    General genResp ->
      putStrLn $ "Received a generic response with status: " ++ show (resStatusCode genResp)
    _ ->
      putStrLn "Received an unexpected response type."

```

To send a message to specific user, use action `WriteChannelPrivate`.

```haskell
tpoClient endpoint (WriteChannelPrivate myChannel "some-user" message)
```

`taipeion-api` implemented not only text messages, but various message types such as image messages, video messages, and more.

Check documentation for more details.

### File Upload

To upload a file, you first need to read its contents into a `ByteString`, then use the `UploadFile` action.

Check the `resFileID` field of the parsed response to get the file id, which is needed to send a media message.

```haskell
-- Example: Uploading a local image
uploadMyImage :: IO ()
uploadMyImage = do
  fileData <- BL.readFile "chiikawa.jpg"
  -- Filename, Extension, Data, IsAsset
  let action = UploadFile myChannel "chiikawa" "jpg" fileData Nothing

  putStrLn "Uploading file..."
  response <- tpoClient apiEndpoint action

  case response of
    UploadFileResponse ufResp -> do
      putStrLn $ "File uploaded successfully! FileID: " ++ unpack (resFileID ufResp)
      -- Now you can use this FileID to send an image message
      let imageMessage = mkImageMessage (Just "吉伊卡哇") "chiikawa.jpg" (resFileID ufResp)
      let sendAction = WriteChannelPrivate myChannel "some-user" imageMessage
      responseMsg <- tpoClient apiEndpoint sendAction
      -- ... response processing ...
    ErrorResponse errResp ->
      putStrLn $ "API Error: " ++ show (resErrorMessage errResp)
    _ ->
      putStrLn "Received an unexpected response type."
```

### Webhook Integration
When your application receives a POST request from a TaipeiON webhook, you can parse the body using the `Data.Aeson.decode` function with the `WebhookPayload` type.

## API Modules

* `TaipeiOn.Client`: Provides the main `tpoClient` function for executing API actions.

* `TaipeiOn.Message`: Contains data types and constructors for creating API request payloads.

* `TaipeiOn.Response:` Contains data types and parsers for handling API responses.

* `TaipeiOn.Webhook.Event`: Contains data types for parsing incoming webhook events.

For detailed information on all available types and functions, please consult the documentation.

## Contributing
Check the [Development Guide](GUIDE.md) for instructions on how to get started.

## License
This project is licensed under the BSD-3-Clause License. See the [LICENSE](LICENSE) file for details.
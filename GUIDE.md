# Development Guide
This document provides guidelines and tips for developers looking to contribute to the taipeiOnApi Haskell library.

## Project Structure

The library is organized into four main modules, each with a distinct responsibility:

* `src/TaipeiOn/Client.hs`: This is the high-level interface for the library. It defines the `Action` sum type, which enumerates *almost-all* possible API calls, and the main `tpoClient` function that executes them.

* `src/TaipeiOn/Message.hs`: This module defines the data types and constructor functions needed to build the JSON payloads for outgoing requests to the API.

* `src/TaipeiOn/Response.hs`: This module defines the data types for parsing the JSON from incoming responses from the API. It includes a sum type `TpoResponse` to encapsulate possible successful and error responses.

* `src/TaipeiOn/Webhook/Event.hs`: This module is dedicated to parsing JSON payloads from incoming webhooks. Its data types model the events sent by the TaipeiON platform to your application.

## Core Concepts

* **Action-Driven Design**: The `TaipeiOn.Client.Action` sum type is the cornerstone of the client. Instead of having a separate function for each API call, we define a single data type that represents different actions. This makes the code organized and easy to extend.

* **Type-Safety**: We use distinct Haskell data types for every JSON object in the API. This, combined with Aeson's ToJSON and FromJSON typeclasses, ensures that we can't accidentally construct a malformed request or misinterpret a response at runtime (as possible).

* **Intelligent Decoding**: The TaipeiOn.Response.decodeTpoResponse function is a higher-order helper that standardizes response parsing. It attempts to parse a successful response, falls back to parsing an API error response, and as a last resort, returns the raw HTTP response. This makes the client robust.

## How to: Add a New API Action

Adding a new client action is a systematic process involving three modules. Let's walk through a hypothetical example: adding a "get user profile" feature.

### Step 1: Define the Request (`Message.hs`)

#### Update ApiPayload
If the new request needs a unique field, add it. For our example, we need a user ID.

Make sure to wrap the field datatype with `Maybe`, so the field would be omitted if not provided.

```haskell
-- In TaipeiOn.Message
data ApiPayload = ApiPayload
  { -- ... existing fields
  , apiUserId        :: Maybe Text -- <-- Add this
  }
```

#### Update `mkEmptyRequest`

Initialize the new field to Nothing.

```haskell
-- In TaipeiOn.Message
mkEmptyRequest :: ApiPayload
mkEmptyRequest = ApiPayload
                        { -- ... existing initializations
                        , apiUserId        = Nothing -- <-- Add this
                        }
```

#### Update `ToJSON` Instance

Add the new field to the JSON object construction.

```haskell
-- In TaipeiOn.Message
instance ToJSON ApiPayload where
    toJSON ApiPayload{..} =
        object $ filter notEmpty
            [ -- ... existing pairs
            , "userId"         .= apiUserId -- <-- Add this
            ]
```

#### Create a Constructor

Write a new **exported** function to build the specific `ApiPayload` for this request.

```haskell
-- In TaipeiOn.Message
mkGetUserProfileRequest :: Text -> ApiPayload
mkGetUserProfileRequest userId = mkEmptyRequest
                                  { apiAsk = "getUserProfile"
                                  , apiUserId = Just userId
                                  }
```

### Step 2: Define the Response (`Response.hs`)

#### Create the `Response` Type

Your new `Action` may results in response type that not yet implemented. Define a new data type that matches the JSON structure of the successful response.

```haskell
-- In TaipeiOn.Response
data ResponseUserProfile = ResponseUserProfile
  { resUserDisplayName :: Text
  , resUserStatus      :: Text
  } deriving (Show, Eq)
```

#### Create a `FromJSON` Instance

You'll need to tell `Aeson` how to parse the JSON into your new type.

```haskell
-- In TaipeiOn.Response
instance FromJSON ResponseUserProfile where
  parseJSON = withObject "ResponseUserProfile" $ \v ->
    ResponseUserProfile <$> v .: "displayName"
                        <*> v .: "statusMessage"
```

#### Update `TpoResponse`

Add a new constructor to the main response sum type.

```haskell
-- In TaipeiOn.Response
data TpoResponse
  = -- ... existing constructors
  | UserProfile           ResponseUserProfile -- <-- Add this
```

#### Create a Decoder

Add a new decoder function that uses the generic `decodeTpoResponse`.

```haskell
-- In TaipeiOn.Response
decodeUserProfileResponse :: H.Response LBS.ByteString -> TpoResponse
decodeUserProfileResponse = decodeTpoResponse UserProfile
```

### Step 3: Integrate into the Client (`Client.hs`)

#### Update `Action`

Add a new constructor to the `Action` sum type.

```haskell
-- In TaipeiOn.Client
data Action
  = -- ... existing actions
  | GetUserProfile Channel Text -- <-- Add this (Channel and UserId)
```

#### Update `mkTpoRequest`

Add a case to handle the new action. This case should call the request constructor you made in Step 1.

```haskell
-- In TaipeiOn.Client
mkTpoRequest :: Action -> Request -> Request
mkTpoRequest action req =
  case action of
    -- ... existing cases
    GetUserProfile chan userId ->
      setTpoChannelHeader chan req
        { requestBody = RequestBodyLBS $AE.encode$ mkGetUserProfileRequest userId
        }
```

#### Update `tpoClient`'s Decoder Logic

Add a final case to the `tpoClient` function to call the correct response decoder from Step 2.

```haskell
-- In TaipeiOn.Client
tpoClient :: String -> Action -> IO TpoResponse
tpoClient endPoint action = do
  -- ... http request logic ...

  case action of
    -- ... existing cases
    GetUserProfile {} -> pure $ decodeUserProfileResponse resp -- <-- Add this
```

By following these three steps, you can cleanly and safely integrate new API functionality into the library.


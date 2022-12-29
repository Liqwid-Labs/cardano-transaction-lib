module Ctl.Internal.Service.Error
  ( BlockfrostError(BlockfrostError)
  , ClientError
      ( ClientHttpError
      , ClientHttpResponseError
      , ClientDecodeJsonError
      , ClientEncodingError
      , ClientOtherError
      )
  , ServiceError
      ( ServiceBlockfrostError
      , ServiceOtherError
      )
  ) where

import Prelude

import Aeson (class DecodeAeson, JsonDecodeError, getField)
import Affjax (Error, printError) as Affjax
import Affjax.StatusCode (StatusCode) as Affjax
import Ctl.Internal.Service.Helpers (aesonObject)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

--------------------------------------------------------------------------------
-- ClientError
--------------------------------------------------------------------------------

data ClientError
  -- | Affjax error
  = ClientHttpError Affjax.Error
  -- | Server responded with HTTP status code outside of 200-299
  | ClientHttpResponseError Affjax.StatusCode ServiceError
  -- | Failed to decode the response
  | ClientDecodeJsonError String JsonDecodeError
  -- | Failed to encode the request
  | ClientEncodingError String
  -- | Any other error
  | ClientOtherError String

-- No `Show` instance of `Affjax.Error`
instance Show ClientError where
  show (ClientHttpError err) =
    "(ClientHttpError "
      <> Affjax.printError err
      <> ")"
  show (ClientHttpResponseError statusCode err) =
    "(ClientHttpResponseError "
      <> show statusCode
      <> " "
      <> show err
      <> ")"
  show (ClientDecodeJsonError jsonStr err) =
    "(ClientDecodeJsonError (" <> show jsonStr <> ") "
      <> show err
      <> ")"
  show (ClientEncodingError err) =
    "(ClientEncodingError "
      <> err
      <> ")"
  show (ClientOtherError err) =
    "(ClientOtherError "
      <> err
      <> ")"

--------------------------------------------------------------------------------
-- ServiceError
--------------------------------------------------------------------------------

data ServiceError
  = ServiceBlockfrostError BlockfrostError
  | ServiceOtherError String

derive instance Generic ServiceError _

instance Show ServiceError where
  show = genericShow

newtype BlockfrostError = BlockfrostError
  { statusCode :: Int
  , error :: String
  , message :: String
  }

derive instance Newtype BlockfrostError _
derive instance Generic BlockfrostError _

instance Show BlockfrostError where
  show = genericShow

instance DecodeAeson BlockfrostError where
  decodeAeson = aesonObject \obj -> do
    statusCode <- getField obj "status_code"
    error <- getField obj "error"
    message <- getField obj "message"
    pure $ BlockfrostError { statusCode, error, message }

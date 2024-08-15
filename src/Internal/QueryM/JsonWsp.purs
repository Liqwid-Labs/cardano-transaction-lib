-- | Provides basics types and operations for working with JSON RPC protocol
-- | used by Ogmios
module Ctl.Internal.QueryM.JsonWsp
  ( JsonWspResponse
  , JsonWspCall
  , mkCallType
  , buildRequest
  , parseJsonWspResponse
  , parseJsonWspResponseId
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , encodeAeson
  , getField
  , getFieldOptional
  )
import Ctl.Internal.QueryM.UniqueId (ListenerId, uniqueId)
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Foreign.Object (Object)
import Record as Record

-- | Structure of all json wsp websocket responses
-- described in: https://ogmios.dev/getting-started/basics/
type JsonWspResponse (a :: Type) =
  { jsonrpc :: String
  -- methodname is not always present if `fault` is not empty
  , method :: Maybe String
  , result :: Maybe a
  , error :: Maybe Aeson
  , id :: ListenerId
  }

-- | A wrapper for tying arguments and response types to request building.
newtype JsonWspCall :: Type -> Type -> Type
newtype JsonWspCall (i :: Type) (o :: Type) = JsonWspCall
  (i -> Effect { body :: Aeson, id :: String })

-- | Creates a "jsonwsp call" which ties together request input and response output types
-- | along with a way to create a request object.
mkCallType
  :: forall (a :: Type) (i :: Type) (o :: Type)
   . EncodeAeson a
  => { method :: String, params :: Maybe (i -> a) }
  -> JsonWspCall i o
mkCallType { method, params } = JsonWspCall $ \i -> do
  id <- uniqueId $ method <> "-"

  -- I just bypass all the type hassle by passing around Aeson valuse
  -- this is pretty sinful, but whatever
  let
    req =
      case params of
        Nothing -> encodeAeson { id, jsonrpc: "2.0", method }
        Just f -> encodeAeson { id, jsonrpc: "2.0", method, params: f i }

  pure { body: req, id }

-- | Create a JsonWsp request body and id
buildRequest
  :: forall (i :: Type) (o :: Type)
   . JsonWspCall i o
  -> i
  -> Effect { body :: Aeson, id :: String }
buildRequest (JsonWspCall c) = c

-- | Polymorphic response parser
parseJsonWspResponse
  :: forall (a :: Type)
   . DecodeAeson a
  => Aeson
  -> Either JsonDecodeError (JsonWspResponse a)
parseJsonWspResponse = aesonObject $ \o -> do
  jsonrpc <- getField o "jsonrpc"
  method <- getFieldOptional o "method"
  result <- getFieldOptional o "result"
  error <- getFieldOptional o "error"
  id <- getField o "id"
  pure
    { jsonrpc
    , method
    , result
    , error
    , id
    }

-- | Parse just ID from the response
parseJsonWspResponseId
  :: Aeson
  -> Either JsonDecodeError ListenerId
parseJsonWspResponseId =
  aesonObject $ flip getField "id"

-- | Helper for assuming we get an object
aesonObject
  :: forall (a :: Type)
   . (Object Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonObject = caseAesonObject (Left (TypeMismatch "expected object"))

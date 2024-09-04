module Ctl.Internal.Types.TokenName
  ( TokenName
  , adaToken
  , getTokenName
  , mkTokenName
  , mkTokenNames
  , tokenNameFromAssetName
  , assetNameName
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , encodeAeson
  , getField
  )
import Contract.Prim.ByteArray (hexToByteArray)
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata)
import Ctl.Internal.Serialization.Types (AssetName) as CSL
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayToHex, byteLength)
import Ctl.Internal.Types.RawBytes (RawBytes)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.BigInt (BigInt)
import Data.Bitraversable (ltraverse)
import Data.Either (Either(Left), note)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested (type (/\))
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (resize)

newtype TokenName = TokenName RawBytes

derive newtype instance Eq TokenName
derive newtype instance FromData TokenName
derive newtype instance FromMetadata TokenName
derive newtype instance ToMetadata TokenName
derive newtype instance Ord TokenName
derive newtype instance ToData TokenName

instance Arbitrary TokenName where
  arbitrary = unsafePartial fromJust <<< mkTokenName <$> resize 32 arbitrary

foreign import _decodeUtf8
  :: forall (r :: Type). Uint8Array -> (String -> r) -> (String -> r) -> r

-- | Corresponds to CurrencySymbol
instance DecodeAeson TokenName where
  decodeAeson = caseAesonObject
    (Left $ TypeMismatch "Expected object")
    ( note (TypeMismatch "Invalid TokenName") <<< mkTokenName
        <=< note (TypeMismatch "Invalid ByteArray") <<< hexToByteArray
        <=< flip getField "unTokenName"
    )

instance EncodeAeson TokenName where
  encodeAeson (TokenName ba) = encodeAeson
    { "unTokenName": byteArrayToHex (unwrap ba) }

instance Show TokenName where
  show (TokenName tn) = "(TokenName " <> show tn <> ")"

getTokenName :: TokenName -> ByteArray
getTokenName (TokenName tokenName) = unwrap tokenName

-- | The empty token name.
adaToken :: TokenName
adaToken = TokenName mempty

-- | Create a `TokenName` from a `ByteArray` since TokenName data constructor is
-- | not exported
mkTokenName :: ByteArray -> Maybe TokenName
mkTokenName byteArr
  | byteLength byteArr <= 32 = pure $ TokenName $ wrap byteArr
  | otherwise = Nothing

foreign import assetNameName :: CSL.AssetName -> ByteArray

tokenNameFromAssetName :: CSL.AssetName -> TokenName
tokenNameFromAssetName = TokenName <<< wrap <<< assetNameName

-- | Creates a Map of `TokenName` and Big Integers from a `Traversable` of 2-tuple
-- | `ByteArray` and Big Integers with the possibility of failure
mkTokenNames
  :: forall (t :: Type -> Type)
   . Traversable t
  => t (ByteArray /\ BigInt)
  -> Maybe (Map TokenName BigInt)
mkTokenNames = traverse (ltraverse mkTokenName) >>> map Map.fromFoldable

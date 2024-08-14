module Ctl.Internal.Contract.MinFee (calculateMinFee) where

import Prelude

import Contract.Prim.ByteArray (byteLength)
import Ctl.Internal.Cardano.Types.ScriptRef (getPlutusScript)
import Ctl.Internal.Cardano.Types.Transaction
  ( Certificate
      ( StakeRegistration
      , StakeDeregistration
      , StakeDelegation
      , PoolRegistration
      , PoolRetirement
      )
  , Transaction
  , UtxoMap
  , _body
  , _certs
  , _collateral
  , _inputs
  , _referenceInputs
  , _withdrawals
  )
import Ctl.Internal.Cardano.Types.Value (Coin)
import Ctl.Internal.Contract (getProtocolParameters)
import Ctl.Internal.Contract.Monad (Contract)
import Ctl.Internal.ProcessConstraints.State (_refScriptsUtxoMap)
import Ctl.Internal.Serialization.Address
  ( addressPaymentCred
  , addressStakeCred
  , rewardAddressToAddress
  , stakeCredentialToKeyHash
  )
import Ctl.Internal.Serialization.MinFee (calculateMinFeeCsl)
import Ctl.Internal.Types.ByteArray (ByteArray(..))
import Ctl.Internal.Types.RewardAddress (unRewardAddress)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Foldable (length, sum)
import Data.Foldable as Foldable
import Data.Lens.Getter ((^.))
import Data.Map (keys, lookup) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Tuple (fst)

-- | Calculate `min_fee` using CSL with protocol parameters from Ogmios.
calculateMinFee :: Transaction -> UtxoMap -> Contract Coin
calculateMinFee tx allUtxos = do
  pparams <- getProtocolParameters
  -- add 1 for wallets that may include an additional stake key signature
  calculateMinFeeCsl
    pparams
    (1 + nInputKeys + nWithdrawalKeys + nCertificateKeys)
    refScriptsSize
    tx
  where
  nInputKeys :: Int
  nInputKeys =
    Array.fromFoldable (tx ^. _body <<< _inputs)
      <> fromMaybe [] (tx ^. _body <<< _collateral)
      # mapMaybe
          ( flip Map.lookup allUtxos
              >=> (unwrap >>> _.address >>> Just)
              >=> addressPaymentCred
              >=> stakeCredentialToKeyHash
          )
      # Set.fromFoldable
      # length

  refScriptsSize :: Int
  refScriptsSize =
    Array.fromFoldable (tx ^. _body <<< _referenceInputs)
      # mapMaybe
          ( flip Map.lookup allUtxos
              >=> (unwrap >>> _.scriptRef)
              -- Note(Przemek, 14th Aug 2024) - native script not supported as ref script input
              >=> getPlutusScript

          )
      <#> (unwrap >>> fst >>> byteLength)
      # sum

  nWithdrawalKeys :: Int
  nWithdrawalKeys =
    case tx ^. _body <<< _withdrawals of
      Just wdrls ->
        Map.keys wdrls
          # Array.fromFoldable
          # mapMaybe
              ( unRewardAddress
                  >>> rewardAddressToAddress
                  >>> addressStakeCred
                  >=> stakeCredentialToKeyHash
              )
          # Set.fromFoldable
          # length
      Nothing -> 0

  nCertificateKeys :: Int
  nCertificateKeys =
    case tx ^. _body <<< _certs of
      Just certs -> Foldable.sum $ nCertPubKeyHashes <$> certs
      Nothing -> 0

  nCertPubKeyHashes :: Certificate -> Int
  nCertPubKeyHashes cert = length $
    case cert of
      StakeRegistration stakeCred ->
        Array.fromFoldable $ stakeCredentialToKeyHash stakeCred
      StakeDeregistration stakeCred ->
        Array.fromFoldable $ stakeCredentialToKeyHash stakeCred
      StakeDelegation stakeCred _ ->
        Array.fromFoldable $ stakeCredentialToKeyHash stakeCred
      PoolRegistration params ->
        Array.cons
          (unwrap <<< unwrap $ params.operator)
          (unwrap <<< unwrap <$> params.poolOwners)
      PoolRetirement { poolKeyHash } -> [ unwrap <<< unwrap $ poolKeyHash ]
      _ -> []

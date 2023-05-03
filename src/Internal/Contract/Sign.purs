module Ctl.Internal.Contract.Sign
  ( signTransaction
  ) where

import Prelude

import Control.Monad.Reader (asks)
import Ctl.Internal.Cardano.Types.Transaction (_body, _inputs, _witnessSet)
import Ctl.Internal.Cardano.Types.Transaction as Transaction
import Ctl.Internal.Contract.Monad (Contract)
import Ctl.Internal.Contract.QueryHandle (getQueryHandle)
import Ctl.Internal.Contract.Wallet
  ( getWalletAddresses
  , getWalletUtxos
  , withWallet
  )
import Ctl.Internal.Helpers (liftedM)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Ctl.Internal.Wallet
  ( Wallet(KeyWallet, Lode, Eternl, Flint, Gero, Nami, NuFi)
  , callCip30Wallet
  )
import Data.Array (elem, fromFoldable)
import Data.Either (hush)
import Data.Lens ((<>~))
import Data.Lens.Getter ((^.))
import Data.Map as Map
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (for_, traverse)
import Data.Tuple.Nested ((/\))
import Effect.Aff (delay, error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw, try)

signTransaction
  :: Transaction.Transaction -> Contract (Maybe Transaction.Transaction)
signTransaction tx = do
  hooks <- asks _.hooks
  for_ hooks.beforeSign (void <<< liftEffect <<< try)
  withWallet case _ of
    Nami nami -> liftAff $ callCip30Wallet nami \nw -> flip nw.signTx tx
    Gero gero -> liftAff $ callCip30Wallet gero \nw -> flip nw.signTx tx
    Flint flint -> liftAff $ callCip30Wallet flint \nw -> flip nw.signTx tx
    Eternl eternl -> liftAff $ callCip30Wallet eternl \nw -> flip nw.signTx tx
    Lode lode -> liftAff $ callCip30Wallet lode \nw -> flip nw.signTx tx
    NuFi nufi -> liftAff $ callCip30Wallet nufi \w -> flip w.signTx tx
    KeyWallet kw -> liftAff do
      witnessSet <- (unwrap kw).signTx tx
      pure $ Just (tx # _witnessSet <>~ witnessSet)

module Frontend.Api.WalletApi where

import Prelude

import Cardano.Wallet as CW
import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff)

import Frontend.Api.Utils (maybeAff)
import Frontend.Capability.LogMessages (class LogMessages)

getBalance :: ∀ m. MonadAff m => LogMessages m => CW.Api -> m (Maybe CW.Cbor)
getBalance = maybeAff <<< CW.getBalance

getChangeAddress :: ∀ m. MonadAff m => LogMessages m => CW.Api -> m (Maybe CW.Cbor)
getChangeAddress = maybeAff <<< CW.getChangeAddress

getNetworkId :: ∀ m. MonadAff m => LogMessages m => CW.Api -> m (Maybe CW.NetworkId)
getNetworkId = maybeAff <<< CW.getNetworkId

getRewardAddresses :: ∀ m. MonadAff m => LogMessages m => CW.Api -> m (Maybe (Array CW.Cbor))
getRewardAddresses = maybeAff <<< CW.getRewardAddresses

getUsedAddresses :: ∀ m. MonadAff m => LogMessages m => CW.Api -> CW.Paginate -> m (Maybe (Array CW.Cbor))
getUsedAddresses api = maybeAff <<< CW.getUsedAddresses api

getUtxos :: ∀ m. MonadAff m => LogMessages m => CW.Api -> Maybe CW.Paginate -> m (Maybe (Array CW.Cbor))
getUtxos api = maybeAff <<< CW.getUtxos api

signTx :: ∀ m. MonadAff m => LogMessages m => CW.Api -> CW.Cbor -> Boolean -> m (Maybe CW.Cbor)
signTx api tx = maybeAff <<< CW.signTx api tx

submitTx :: ∀ m. MonadAff m => LogMessages m => CW.Api -> CW.Cbor -> m (Maybe CW.Cbor)
submitTx api = maybeAff <<< CW.submitTx api
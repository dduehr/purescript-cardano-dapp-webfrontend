module Frontend.Capability.Domain.Wallet where

import Prelude

import Cardano.Wallet (NetworkId) as CW
import Control.Monad.Maybe.Trans (runMaybeT)
import Csl (Address, BigNum, Tx, TxUnspentOut, TxWitnessSet) as CS
import Data.Maybe (Maybe(..))

import Frontend.Api.Infrastructure.Cip30.WalletApi
  ( class ManageWalletXpi
  , getWalletBalance
  , getWalletChangeAddress
  , getWalletNetworkId
  , getWalletRewardAddresses
  , getWalletUsedAddresses
  , getWalletUtxos
  , signTx
  , submitTx
  ) as WalletApi
import Frontend.Data.Tx (TxId)
import Frontend.Data.Wallet (WalletApi)

getWalletBalance :: ∀ m. WalletApi.ManageWalletXpi m => WalletApi -> m (Maybe CS.BigNum)
getWalletBalance = runMaybeT <<< WalletApi.getWalletBalance

getWalletChangeAddress :: ∀ m. WalletApi.ManageWalletXpi m => WalletApi -> m (Maybe CS.Address)
getWalletChangeAddress = runMaybeT <<< WalletApi.getWalletChangeAddress

getWalletNetworkId :: ∀ m. WalletApi.ManageWalletXpi m => WalletApi -> m (Maybe CW.NetworkId)
getWalletNetworkId = runMaybeT <<< WalletApi.getWalletNetworkId

getWalletRewardAddresses :: ∀ m. WalletApi.ManageWalletXpi m => WalletApi -> m (Maybe (Array CS.Address))
getWalletRewardAddresses = runMaybeT <<< WalletApi.getWalletRewardAddresses

getWalletUsedAddresses :: ∀ m. WalletApi.ManageWalletXpi m => WalletApi -> m (Maybe (Array CS.Address))
getWalletUsedAddresses api = runMaybeT $ WalletApi.getWalletUsedAddresses api { limit: 10, page: 0 }

getWalletUtxos :: ∀ m. WalletApi.ManageWalletXpi m => WalletApi -> m (Maybe (Array CS.TxUnspentOut))
getWalletUtxos api = runMaybeT $ WalletApi.getWalletUtxos api Nothing

signTx :: ∀ m. WalletApi.ManageWalletXpi m => WalletApi -> CS.Tx -> m (Maybe CS.TxWitnessSet)
signTx api tx = runMaybeT $ WalletApi.signTx api tx false

submitTx :: ∀ m. WalletApi.ManageWalletXpi m => WalletApi -> CS.Tx -> m (Maybe TxId)
submitTx api = runMaybeT <<< WalletApi.submitTx api
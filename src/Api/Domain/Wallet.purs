module Frontend.Api.Domain.Wallet where

import Prelude

import Cardano.Wallet (NetworkId) as CW
import Csl (Address, BigNum, Tx, TxUnspentOut, TxWitnessSet) as CS
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift) as H

import Frontend.Data.Tx (TxId)
import Frontend.Data.Wallet (WalletApi)

class Monad m <= ManageWallet m where
  getWalletNetworkId :: WalletApi -> m (Maybe CW.NetworkId)
  getWalletBalance :: WalletApi -> m (Maybe CS.BigNum)
  getWalletChangeAddress :: WalletApi -> m (Maybe CS.Address)
  getWalletRewardAddresses :: WalletApi -> m (Maybe (Array CS.Address))
  getWalletUsedAddresses :: WalletApi -> m (Maybe (Array CS.Address))
  getWalletUtxos :: WalletApi -> m (Maybe (Array CS.TxUnspentOut))
  signTx :: WalletApi -> CS.Tx -> m (Maybe CS.TxWitnessSet)
  submitTx :: WalletApi -> CS.Tx -> m (Maybe TxId)

instance manageWalletHalogenM ::
  ManageWallet m =>
  ManageWallet (H.HalogenM state action slots output m) where
  getWalletNetworkId = H.lift <<< getWalletNetworkId
  getWalletBalance = H.lift <<< getWalletBalance
  getWalletChangeAddress = H.lift <<< getWalletChangeAddress
  getWalletRewardAddresses = H.lift <<< getWalletRewardAddresses
  getWalletUsedAddresses = H.lift <<< getWalletUsedAddresses
  getWalletUtxos = H.lift <<< getWalletUtxos
  signTx api = H.lift <<< signTx api
  submitTx api = H.lift <<< submitTx api
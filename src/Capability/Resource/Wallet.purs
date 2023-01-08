module Frontend.Capability.Resource.Wallet where

import Prelude

import Cardano.Wallet (NetworkId) as CW
import Csl (Address, BigNum, Tx, TxUnspentOut, TxWitnessSet) as CS
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift) as H

import Frontend.Data.Tx (TxId)
import Frontend.Data.Wallet (WalletApi)

class Monad m <= ManageWallet m where
  getNetworkId :: WalletApi -> m (Maybe CW.NetworkId)
  getBalance :: WalletApi -> m (Maybe CS.BigNum)
  getChangeAddress :: WalletApi -> m (Maybe CS.Address)
  getRewardAddresses :: WalletApi -> m (Maybe (Array CS.Address))
  getUsedAddresses :: WalletApi -> m (Maybe (Array CS.Address))
  getUtxos :: WalletApi -> m (Maybe (Array CS.TxUnspentOut))
  signTx :: WalletApi -> CS.Tx -> m (Maybe CS.TxWitnessSet)
  submitTx :: WalletApi -> CS.Tx -> m (Maybe TxId)

instance manageWalletHalogenM ::
  ManageWallet m =>
  ManageWallet (H.HalogenM state action slots output m) where
  getNetworkId = H.lift <<< getNetworkId
  getBalance = H.lift <<< getBalance
  getChangeAddress = H.lift <<< getChangeAddress
  getRewardAddresses = H.lift <<< getRewardAddresses
  getUsedAddresses = H.lift <<< getUsedAddresses
  getUtxos = H.lift <<< getUtxos
  signTx api = H.lift <<< signTx api
  submitTx api = H.lift <<< submitTx api
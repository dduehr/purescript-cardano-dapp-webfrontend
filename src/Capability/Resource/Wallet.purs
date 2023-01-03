module Frontend.Capability.Resource.Wallet where

import Prelude

import Cardano.Wallet (Api, WalletName) as CW
import Csl as CS
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift) as H

import Frontend.Data.Tx (TxId)

class Monad m <= ManageWallet m where
  enableWallet :: CW.WalletName -> m (Maybe CW.Api)
  getTxUnspentOuts :: CW.Api -> m (Maybe CS.TxUnspentOuts)
  getChangeAddress :: CW.Api -> m (Maybe CS.Address)
  signTx :: CW.Api -> CS.Tx -> m (Maybe CS.TxWitnessSet)
  submitTx :: CW.Api -> CS.Tx -> m (Maybe TxId)

instance manageWalletHalogenM :: ManageWallet m => ManageWallet (H.HalogenM state action slots output m) where
  enableWallet = H.lift <<< enableWallet
  getTxUnspentOuts = H.lift <<< getTxUnspentOuts
  getChangeAddress  = H.lift <<< getChangeAddress
  signTx walletApi = H.lift <<< signTx walletApi
  submitTx walletApi = H.lift <<< submitTx walletApi
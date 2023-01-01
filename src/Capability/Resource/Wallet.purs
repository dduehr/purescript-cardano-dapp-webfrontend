module Example.Capability.Resource.Wallet where

import Prelude

import Cardano.Wallet (Api, WalletName) as CW
import Csl as Csl
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift) as H

import Example.Data.Transaction (TxId)

class Monad m <= ManageWallet m where
  enableWallet :: CW.WalletName -> m (Maybe CW.Api)
  getTxUnspentOuts :: CW.Api -> m (Maybe Csl.TxUnspentOuts)
  getChangeAddress :: CW.Api -> m (Maybe Csl.Address)
  signTx :: CW.Api -> Csl.TxBody -> m (Maybe Csl.TxWitnessSet)
  submitTx :: CW.Api -> Csl.TxBody -> Csl.TxWitnessSet -> m (Maybe TxId)

instance manageWalletHalogenM :: ManageWallet m => ManageWallet (H.HalogenM state action slots output m) where
  enableWallet = H.lift <<< enableWallet
  getTxUnspentOuts = H.lift <<< getTxUnspentOuts
  getChangeAddress  = H.lift <<< getChangeAddress
  signTx walletApi = H.lift <<< signTx walletApi
  submitTx walletApi txBody = H.lift <<< submitTx walletApi txBody
module Example.Capability.Resource.WebPage where

import Prelude

import Cardano.Wallet (WalletName) as CW
import Data.Maybe (Maybe)

import Example.Data.Wallet (Wallet)

class Monad m <= ManageWebPage m where
  getWallet :: CW.WalletName -> m (Maybe Wallet)
  availableWallets :: m (Maybe (Array Wallet))
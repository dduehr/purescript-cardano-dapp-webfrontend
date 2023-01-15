module Frontend.Api.Infrastructure.Cip30.Browser where

import Prelude

import Cardano.Wallet (Api, WalletName) as CW
import Control.Monad.Maybe.Trans (MaybeT)

class Monad m <= ManageBrowser m where
  getAvailableWallets :: MaybeT m (Array CW.WalletName)
  isWalletAvailable :: CW.WalletName -> MaybeT m Boolean
  getWalletApiVersion :: CW.WalletName -> MaybeT m String
  getWalletIcon :: CW.WalletName -> MaybeT m String
  getWalletName :: CW.WalletName -> MaybeT m String
  enableWalletApi :: CW.WalletName -> MaybeT m CW.Api


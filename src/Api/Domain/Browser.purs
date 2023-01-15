module Frontend.Api.Domain.Browser where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift) as H

import Frontend.Data.Wallet (Wallet, WalletId)

class Monad m <= ManageBrowser m where
  getWallets :: m (Maybe (NonEmptyArray Wallet))
  getWallet :: WalletId -> m (Maybe Wallet)
  enableWallet :: WalletId -> m (Boolean)
  disableWallet :: m (Unit)

instance manageBrowserHalogenM ::
  ManageBrowser m =>
  ManageBrowser (H.HalogenM state action slots output m) where
  getWallets = H.lift getWallets
  getWallet = H.lift <<< getWallet
  enableWallet = H.lift <<< enableWallet
  disableWallet = H.lift disableWallet
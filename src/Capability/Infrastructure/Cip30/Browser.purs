module Frontend.Capability.Infrastructure.Cip30.Browser where

import Prelude

import Cardano.Wallet (Api, WalletName, availableWallets, enable, getApiVersion, getIcon, getName, isWalletAvailable) as CW
import Control.Monad.Maybe.Trans (MaybeT(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

import Frontend.Api.LogMessages (class LogMessages)
import Frontend.Capability.Utils (maybeEffect, maybeAff)

getAvailableWallets :: ∀ m. MonadEffect m => LogMessages m => MaybeT m (Array CW.WalletName)
getAvailableWallets = MaybeT $ maybeEffect CW.availableWallets

isWalletAvailable :: ∀ m. MonadEffect m => LogMessages m => CW.WalletName -> MaybeT m Boolean
isWalletAvailable = MaybeT <<< maybeEffect <<< CW.isWalletAvailable

getWalletXpiVersion :: ∀ m. MonadEffect m => LogMessages m => CW.WalletName -> MaybeT m String
getWalletXpiVersion = MaybeT <<< maybeEffect <<< CW.getApiVersion

getWalletIcon :: ∀ m. MonadEffect m => LogMessages m => CW.WalletName -> MaybeT m String
getWalletIcon = MaybeT <<< maybeEffect <<< CW.getIcon

getWalletName :: ∀ m. MonadEffect m => LogMessages m => CW.WalletName -> MaybeT m String
getWalletName = MaybeT <<< maybeEffect <<< CW.getName

enableWalletXpi :: ∀ m. MonadAff m => LogMessages m => CW.WalletName -> MaybeT m CW.Api
enableWalletXpi = MaybeT <<< maybeAff <<< CW.enable
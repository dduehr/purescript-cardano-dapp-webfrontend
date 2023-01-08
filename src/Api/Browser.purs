module Frontend.Api.Browser where

import Prelude

import Cardano.Wallet as CW
import Data.Array (filter)
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)

import Frontend.Api.Utils (maybeAff, maybeEffect)
import Frontend.Capability.LogMessages (class LogMessages)
import Frontend.Data.Wallet (WalletId)
import Frontend.Store (Action(..), Store) as Store

availableWallets :: ∀ m. MonadEffect m => LogMessages m => MonadStore Store.Action Store.Store m => m (Maybe (NonEmptyArray WalletId))
availableWallets = do
  mbWalletNames <- maybeEffect CW.availableWallets
  { blacklistedWallets } <- getStore
  let mbWalletIds = filter (\id -> show id `not elem` blacklistedWallets) <$> map wrap <$> mbWalletNames
  let mbNonEmptyWalletIds = join $ fromArray <$> mbWalletIds
  pure mbNonEmptyWalletIds

getApiVersion :: ∀ m. MonadEffect m => LogMessages m => WalletId -> m (Maybe String)
getApiVersion = maybeEffect <<< CW.getApiVersion <<< unwrap

getIcon :: ∀ m. MonadEffect m => LogMessages m => WalletId -> m (Maybe String)
getIcon = maybeEffect <<< CW.getIcon <<< unwrap

getName :: ∀ m. MonadEffect m => LogMessages m => WalletId -> m (Maybe String)
getName = maybeEffect <<< CW.getName <<< unwrap

isWalletAvailable :: ∀ m. MonadEffect m => LogMessages m => WalletId -> m (Maybe Boolean)
isWalletAvailable = maybeEffect <<< CW.isWalletAvailable <<< unwrap

enable :: ∀ m. MonadAff m => LogMessages m => MonadStore Store.Action Store.Store m => WalletId -> m (Maybe CW.Api)
enable id = do
  mbApi <- maybeAff <<< CW.enable $ unwrap id
  updateStore $ case mbApi of
    Just api -> Store.EnableWallet { id, api }
    _ -> Store.DisableWallet
  pure mbApi

disable :: ∀ m. MonadAff m => LogMessages m => MonadStore Store.Action Store.Store m => m (Unit)
disable = updateStore Store.DisableWallet
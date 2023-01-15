module Frontend.Capability.Domain.Browser where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Array (filter)
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Traversable (sequence, traverse)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)

import Frontend.Api.Infrastructure.Cip30.Browser
  ( class ManageBrowser
  , enableWalletXpi
  , getAvailableWallets
  , getWalletXpiVersion
  , getWalletIcon
  , getWalletName
  , isWalletAvailable
  ) as Browser
import Frontend.Data.Wallet (Wallet, WalletId(..))
import Frontend.Store (Action(..), Store) as Store

getWallets :: ∀ m. Browser.ManageBrowser m => MonadStore Store.Action Store.Store m => m (Maybe (NonEmptyArray Wallet))
getWallets = runMaybeT $ do
  walletIds <- map WalletId <$> Browser.getAvailableWallets
  { blacklistedWallets } <- getStore
  let filteredWalletIds = filter (\id -> show id `not elem` blacklistedWallets) walletIds
  wallets <- bindFlipped fromArray <$> sequence <$> traverse (lift <<< getWallet) filteredWalletIds
  MaybeT $ pure $ wallets

getWallet :: ∀ m. Browser.ManageBrowser m => WalletId -> m (Maybe Wallet)
getWallet id = runMaybeT $ do
  let id' = unwrap id
  isWalletAvailable <- Browser.isWalletAvailable id'
  if isWalletAvailable then do
    name <- Browser.getWalletName id'
    apiVersion <- Browser.getWalletXpiVersion id'
    icon <- Browser.getWalletIcon id'
    pure { id, name, apiVersion, icon }
  else
    MaybeT $ pure Nothing

enableWallet :: ∀ m. Browser.ManageBrowser m => MonadStore Store.Action Store.Store m => WalletId -> m (Boolean)
enableWallet id = do
  let id' = unwrap id
  mbXpi <- runMaybeT $ Browser.enableWalletXpi id'
  updateStore case mbXpi of
    Just api -> Store.EnableWallet { id, api }
    Nothing -> Store.DisableWallet
  pure $ isJust mbXpi

disableWallet :: ∀ m. Browser.ManageBrowser m => MonadStore Store.Action Store.Store m => m (Unit)
disableWallet = updateStore Store.DisableWallet
module Frontend.Store where

import Csl (TxBuilderConfig) as CS
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds)

import Frontend.Data.Wallet (WalletCredentials)

type Store =
  { mbWalletCredentials :: Maybe WalletCredentials
  , mbWalletReloadSec :: Maybe Seconds
  , mbTxBuilderConfig :: Maybe CS.TxBuilderConfig
  , blacklistedWallets :: Array String
  }

data Action
  = EnableWallet WalletCredentials
  | DisableWallet

reduce :: Store -> Action -> Store
reduce store = case _ of
  EnableWallet walletCredentials ->
    store { mbWalletCredentials = Just walletCredentials }
  DisableWallet ->
    store { mbWalletCredentials = Nothing }
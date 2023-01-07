module Frontend.Store where

import Cardano.Wallet (Api, WalletName) as CW
import Csl (TxBuilderConfig) as CS
import Data.Maybe (Maybe(..))

type Store =
  { blacklist :: Array String
  , wallet :: Maybe Wallet
  , txBuilderConfig :: Maybe CS.TxBuilderConfig
  }

type Wallet =
  { id :: CW.WalletName
  , api :: CW.Api
  }

data Action
  = EnableWallet Wallet
  | DisableWallet

reduce :: Store -> Action -> Store
reduce store = case _ of
  EnableWallet wallet ->
    store { wallet = Just wallet }
  DisableWallet ->
    store { wallet = Nothing }
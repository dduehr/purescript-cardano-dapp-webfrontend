module Frontend.Data.Wallet where

import Cardano.Wallet (WalletName) as CW

type Wallet =
  { id :: CW.WalletName
  , name :: String
  , apiVersion :: String
  , icon :: String
  }
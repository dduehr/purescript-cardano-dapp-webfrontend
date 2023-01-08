module Frontend.Data.WalletName where

import Cardano.Wallet (WalletName(..)) as CW

unwrap :: CW.WalletName -> String
unwrap (CW.WalletName name) = name
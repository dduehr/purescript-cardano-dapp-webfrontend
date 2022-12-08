module Qapla.Api.WalletName (unwrap) where

import Cardano.Wallet (WalletName(..))

unwrap :: WalletName -> String
unwrap (WalletName name) = name



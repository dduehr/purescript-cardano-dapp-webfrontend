module Frontend.Data.Wallet where

import Prelude

import Cardano.Wallet (Api, WalletName) as CW
import Data.Newtype (class Newtype, unwrap)

import Frontend.Data.WalletName (unwrap) as WalletName

newtype WalletId = WalletId CW.WalletName

derive instance newtypeWalletId :: Newtype WalletId _

instance showWalletId :: Show WalletId where
  show = WalletName.unwrap <<< unwrap

instance eqWalletId :: Eq WalletId where
  eq a b = show a == show b

type WalletApi = CW.Api

type Wallet =
  { id :: WalletId
  , name :: String
  , apiVersion :: String
  , icon :: String
  }

type WalletCredentials =
  { id :: WalletId
  , api :: WalletApi
  }


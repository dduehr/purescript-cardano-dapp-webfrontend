module Example.Capability.Resource.WebPage where

import Prelude

import Data.Maybe (Maybe)

import Example.Data.Wallet (Wallet)

class Monad m <= ManageWebPage m where
  availableWallets :: m (Maybe (Array Wallet))
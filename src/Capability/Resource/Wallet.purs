module Example.Capability.Resource.Wallet where

import Prelude

import Cardano.Wallet (Api, WalletName) as CW
import Control.Monad.Except.Trans (ExceptT)

type Error = String

-- FIXME: Statt "ExceptT ..."" besser "m (Either Error CW.Api)"?
class Monad m <= ManageWallet m where
  enableWallet :: CW.WalletName -> ExceptT Error m CW.Api
module Example.Capability.Resource.Contract where

import Prelude

import Cardano.Wallet (Api) as CW
import Control.Monad.Except.Trans (ExceptT)
import Csl (Address, TxBuilderConfig) as Csl

type SendAdaToContractFields =
  { contractAddress :: Csl.Address
  -- TBD ...
  }

type SendTokenToContractFields =
  { contractAddress :: Csl.Address
  -- TBD ...
  }

type RedeemAdaFromContractFields =
  { contractAddress :: Csl.Address
  -- TBD ...
  }

type RedeemTokenFromContractFields =
  { contractAddress :: Csl.Address
  -- TBD ...
  }

type Error = String
type TxId  = String

class Monad m <= ManageContract m where
  sendAdaToContract :: CW.Api -> Csl.TxBuilderConfig -> SendAdaToContractFields -> ExceptT Error m TxId
  sendTokenToContract :: CW.Api -> Csl.TxBuilderConfig -> SendTokenToContractFields -> ExceptT Error m TxId
  redeemAdaFromContract :: CW.Api -> Csl.TxBuilderConfig -> RedeemAdaFromContractFields -> ExceptT Error m TxId
  redeemTokenFromContract :: CW.Api -> Csl.TxBuilderConfig -> RedeemTokenFromContractFields -> ExceptT Error m TxId

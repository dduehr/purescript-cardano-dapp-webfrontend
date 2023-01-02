module Frontend.Capability.Resource.TxBuilder where

import Prelude

import Csl (Address, BigNum, TxBody, TxBuilder, TxUnspentOuts) as CS
import Data.Maybe (Maybe)

class Monad m <= ManageTxBuilder m where
  new :: m (Maybe CS.TxBuilder)
  addOut :: CS.TxBuilder -> CS.Address -> CS.BigNum -> m Unit
  addInsFrom ::CS.TxBuilder -> CS.TxUnspentOuts -> m Unit
  addChangeIfNedded :: CS.TxBuilder -> CS.Address -> m Boolean
  build :: CS.TxBuilder -> m CS.TxBody

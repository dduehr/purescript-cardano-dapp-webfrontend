module Example.Capability.Resource.TxBuilder where

import Prelude

import Csl (Address, BigNum, TxBody, TxBuilder, TxUnspentOuts) as Csl
import Data.Maybe (Maybe)

class Monad m <= ManageTxBuilder m where
  new :: m (Maybe Csl.TxBuilder)
  addOut :: Csl.TxBuilder -> Csl.Address -> Csl.BigNum -> m Unit
  addInsFrom ::Csl.TxBuilder -> Csl.TxUnspentOuts -> m Unit
  addChangeIfNedded :: Csl.TxBuilder -> Csl.Address -> m Boolean
  build :: Csl.TxBuilder -> m Csl.TxBody

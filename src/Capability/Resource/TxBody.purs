module Frontend.Capability.Resource.TxBody where

import Prelude

import Csl (TxBody, TxWitnessSet) as Csl

class Monad m <= ManageTxBody m where
  toUnsignedHex :: Csl.TxBody -> m String
  toSignedHex :: Csl.TxBody -> Csl.TxWitnessSet -> m String


module Frontend.Capability.Resource.TxWitnessSet where

import Prelude

import Csl (TxWitnessSet) as CS

class Monad m <= ManageTxWitnessSet m where
  new :: m (CS.TxWitnessSet)


module Example.Capability.Resource.TxWitnessSet where

import Prelude

import Csl (TxWitnessSet) as Csl
import Data.Maybe (Maybe)

class Monad m <= ManageTxWitnessSet m where
  fromHex :: String -> m (Maybe Csl.TxWitnessSet)


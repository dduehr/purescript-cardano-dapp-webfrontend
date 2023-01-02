module Frontend.Capability.Resource.AuxiliaryData where

import Prelude

import Csl (AuxiliaryData) as CS

class Monad m <= ManageAuxiliaryData m where
  new :: m (CS.AuxiliaryData)
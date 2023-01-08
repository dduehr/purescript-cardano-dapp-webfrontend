module Frontend.Api.TxWitnessSet where

import Csl as CS
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect)

import Frontend.Api.Utils (maybeEffect)
import Frontend.Capability.LogMessages (class LogMessages)

new :: ∀ m. MonadEffect m => LogMessages m => m (Maybe CS.TxWitnessSet)
new = maybeEffect CS.txWitnessSet.new

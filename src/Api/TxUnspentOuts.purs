module Frontend.Api.TxUnspentOuts where

import Prelude

import Csl as CS
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect)

import Frontend.Api.Utils (maybeEffect)
import Frontend.Capability.LogMessages (class LogMessages)

new :: ∀ m. MonadEffect m => LogMessages m => Array CS.TxUnspentOut -> m (Maybe CS.TxUnspentOuts)
new = maybeEffect <<< CS.toMutableList
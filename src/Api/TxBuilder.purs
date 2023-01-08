module Frontend.Api.TxBuilder where

import Prelude

import Csl as CS
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect)

import Frontend.Api.Utils (maybeEffect)
import Frontend.Capability.LogMessages (class LogMessages)

new :: ∀ m. MonadEffect m => LogMessages m => CS.TxBuilderConfig -> m (Maybe CS.TxBuilder)
new = maybeEffect <<< CS.txBuilder.new

addChangeIfNeeded :: ∀ m. MonadEffect m => LogMessages m => CS.TxBuilder -> CS.Address -> m (Maybe Boolean)
addChangeIfNeeded txBuilder = maybeEffect <<< CS.txBuilder.addChangeIfNeeded txBuilder

addInsFrom :: ∀ m. MonadEffect m => LogMessages m => CS.TxBuilder -> CS.TxUnspentOuts -> Number -> m (Maybe Unit)
addInsFrom txBuilder txUnspentOuts = maybeEffect <<< CS.txBuilder.addInsFrom txBuilder txUnspentOuts

addOut :: ∀ m. MonadEffect m => LogMessages m => CS.TxBuilder -> CS.TxOut -> m (Maybe Unit)
addOut txBuilder = maybeEffect <<< CS.txBuilder.addOut txBuilder

build :: ∀ m. MonadEffect m => LogMessages m => CS.TxBuilder -> m (Maybe CS.TxBody)
build = maybeEffect <<< CS.txBuilder.build
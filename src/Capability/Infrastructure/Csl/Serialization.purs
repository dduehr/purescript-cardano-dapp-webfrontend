module Frontend.Capability.Infrastructure.Csl.Serialization where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..))
import Csl as CS
import Effect.Class (class MonadEffect)

import Frontend.Api.LogMessages (class LogMessages)
import Frontend.Capability.Utils (maybeEffect)

newAuxiliaryData :: ∀ m. MonadEffect m => LogMessages m => MaybeT m CS.AuxiliaryData
newAuxiliaryData = MaybeT $ maybeEffect CS.auxiliaryData.new

newTxUnspentOuts :: ∀ m. MonadEffect m => LogMessages m => Array CS.TxUnspentOut -> MaybeT m CS.TxUnspentOuts
newTxUnspentOuts = MaybeT <<< maybeEffect <<< CS.toMutableList

newWitnessSet :: ∀ m. MonadEffect m => LogMessages m => MaybeT m CS.TxWitnessSet
newWitnessSet = MaybeT $ maybeEffect CS.txWitnessSet.new

newTxBuilder :: ∀ m. MonadEffect m => LogMessages m => CS.TxBuilderConfig -> MaybeT m CS.TxBuilder
newTxBuilder = MaybeT <<< maybeEffect <<< CS.txBuilder.new

txBuilderAddChangeIfNeeded :: ∀ m. MonadEffect m => LogMessages m => CS.TxBuilder -> CS.Address -> MaybeT m Boolean
txBuilderAddChangeIfNeeded txBuilder = MaybeT <<< maybeEffect <<< CS.txBuilder.addChangeIfNeeded txBuilder

txBuilderAddInsFrom :: ∀ m. MonadEffect m => LogMessages m => CS.TxBuilder -> CS.TxUnspentOuts -> Number -> MaybeT m Unit
txBuilderAddInsFrom txBuilder txUnspentOuts = MaybeT <<< maybeEffect <<< CS.txBuilder.addInsFrom txBuilder txUnspentOuts

txBuilderAddOut :: ∀ m. MonadEffect m => LogMessages m => CS.TxBuilder -> CS.TxOut -> MaybeT m Unit
txBuilderAddOut txBuilder = MaybeT <<< maybeEffect <<< CS.txBuilder.addOut txBuilder

txBuilderBuild :: ∀ m. MonadEffect m => LogMessages m => CS.TxBuilder -> MaybeT m CS.TxBody
txBuilderBuild = MaybeT <<< maybeEffect <<< CS.txBuilder.build

txOutSetDataHash :: ∀ m. MonadEffect m => LogMessages m => CS.TxOut -> CS.DataHash -> MaybeT m Unit
txOutSetDataHash txOut = MaybeT <<< maybeEffect <<< CS.txOut.setDataHash txOut
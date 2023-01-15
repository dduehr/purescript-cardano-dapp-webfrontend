module Frontend.Api.Infrastructure.Csl.Serialization where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT)
import Csl (Address, AuxiliaryData, TxBuilder, TxBody, TxBuilderConfig, TxOut, TxUnspentOut, TxUnspentOuts, TxWitnessSet) as CS

class Monad m <= ManageSerialization m where
  newAuxiliaryData :: MaybeT m CS.AuxiliaryData
  newTxUnspentOuts :: Array CS.TxUnspentOut -> MaybeT m CS.TxUnspentOuts
  newWitnessSet :: MaybeT m CS.TxWitnessSet
  newTxBuilder :: CS.TxBuilderConfig -> MaybeT m CS.TxBuilder
  txBuilderAddChangeIfNeeded :: CS.TxBuilder -> CS.Address -> MaybeT m Boolean
  txBuilderAddInsFrom :: CS.TxBuilder -> CS.TxUnspentOuts -> Number -> MaybeT m Unit
  txBuilderAddOut :: CS.TxBuilder -> CS.TxOut -> MaybeT m Unit
  txBuilderBuild :: CS.TxBuilder -> MaybeT m CS.TxBody


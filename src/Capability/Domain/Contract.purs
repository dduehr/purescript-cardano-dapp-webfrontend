module Frontend.Capability.Domain.Contract where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Csl (hashPlutusData, tx, txOut, value) as CS
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Halogen.Store.Monad (class MonadStore, getStore)

import Frontend.Api.Domain.Contract (RedeemAdaFromContractFields, RedeemTokenFromContractFields, SendAdaToContractFields, SendTokenToContractFields)
import Frontend.Api.Infrastructure.Cip30.WalletApi
  ( class ManageWalletApi
  , getWalletChangeAddress
  , getWalletUtxos
  , signTx
  , submitTx
  ) as WalletApi
import Frontend.Api.Infrastructure.Csl.Serialization
  ( class ManageSerialization
  , newAuxiliaryData
  , newTxBuilder
  , newTxUnspentOuts
  , newWitnessSet
  , txBuilderAddChangeIfNeeded
  , txBuilderAddInsFrom
  , txBuilderAddOut
  , txBuilderBuild
  , txOutSetDataHash
  ) as Serialization
import Frontend.Api.LogMessages (class LogMessages, logMessage)
import Frontend.Data.Tx (TxId)
import Frontend.Data.Wallet (WalletApi)
import Frontend.Store (Action, Store) as Store

sendAdaToContract
  :: ∀ m
   . WalletApi.ManageWalletApi m
  => Serialization.ManageSerialization m
  => LogMessages m
  => MonadStore Store.Action Store.Store m
  => WalletApi
  -> SendAdaToContractFields
  -> m (Maybe TxId)
sendAdaToContract api fields = runMaybeT $ do
  txBuilderConfig <- MaybeT $ _.mbTxBuilderConfig <$> getStore
  txBuilder <- Serialization.newTxBuilder txBuilderConfig
  txUnspentOutArray <- WalletApi.getWalletUtxos api Nothing
  txUnspentOuts <- Serialization.newTxUnspentOuts txUnspentOutArray
  let txOut = CS.txOut.new fields.contractAddress (CS.value.new fields.lovelaceAmount)
  for_ fields.mbPlutusData \plutusData -> do
    Serialization.txOutSetDataHash txOut (CS.hashPlutusData plutusData)
  Serialization.txBuilderAddOut txBuilder txOut
  Serialization.txBuilderAddInsFrom txBuilder txUnspentOuts (toNumber 1)
  changeAddress <- WalletApi.getWalletChangeAddress api
  changeAdded <- Serialization.txBuilderAddChangeIfNeeded txBuilder changeAddress
  if changeAdded then do
    txBody <- Serialization.txBuilderBuild txBuilder
    txWitnessSetEmpty <- Serialization.newWitnessSet
    auxiliaryDataEmpty <- Serialization.newAuxiliaryData
    let txUnsigned = CS.tx.new txBody txWitnessSetEmpty auxiliaryDataEmpty
    txWitnessSetSigned <- WalletApi.signTx api txUnsigned false
    let txSigned = CS.tx.new txBody txWitnessSetSigned auxiliaryDataEmpty
    WalletApi.submitTx api txSigned
  else do
    lift $ logMessage "Failed to add change"
    MaybeT $ pure Nothing

sendTokenToContract :: ∀ m. Monad m => WalletApi -> SendTokenToContractFields -> m (Maybe TxId)
sendTokenToContract _ _ = pure Nothing

redeemAdaFromContract :: ∀ m. Monad m => WalletApi -> RedeemAdaFromContractFields -> m (Maybe TxId)
redeemAdaFromContract _ _ = pure Nothing

redeemTokenFromContract :: ∀ m. Monad m => WalletApi -> RedeemTokenFromContractFields -> m (Maybe TxId)
redeemTokenFromContract _ _ = pure Nothing
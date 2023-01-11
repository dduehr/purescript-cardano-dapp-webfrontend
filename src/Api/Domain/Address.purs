module Frontend.Api.Domain.Address where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Csl (tx, txOut, value) as CS
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Halogen.Store.Monad (class MonadStore, getStore)

import Frontend.Capability.Domain.Address (SendAdaToAddressFields, SendTokenToAddressFields)
import Frontend.Capability.Infrastructure.Cip30.WalletApi
  ( class ManageWalletApi
  , getWalletChangeAddress
  , getWalletUtxos
  , signTx
  , submitTx
  ) as WalletApi
import Frontend.Capability.Infrastructure.Csl.Serialization
  ( class ManageSerialization
  , newAuxiliaryData
  , newTxBuilder
  , newTxUnspentOuts
  , newWitnessSet
  , txBuilderAddChangeIfNeeded
  , txBuilderAddInsFrom
  , txBuilderAddOut
  , txBuilderBuild
  ) as Serialization
import Frontend.Capability.LogMessages (class LogMessages, logMessage)
import Frontend.Data.Wallet (WalletApi)
import Frontend.Data.Tx (TxId)
import Frontend.Store (Action, Store) as Store

sendAdaToAddress
  :: ∀ m
   . WalletApi.ManageWalletApi m
  => Serialization.ManageSerialization m
  => LogMessages m
  => MonadStore Store.Action Store.Store m
  => WalletApi
  -> SendAdaToAddressFields
  -> m (Maybe TxId)
sendAdaToAddress api fields = runMaybeT $ do
  txBuilderConfig <- MaybeT $ _.mbTxBuilderConfig <$> getStore
  txBuilder <- Serialization.newTxBuilder txBuilderConfig
  txUnspentOutArray <- WalletApi.getWalletUtxos api Nothing
  txUnspentOuts <- Serialization.newTxUnspentOuts txUnspentOutArray
  let txOut = CS.txOut.new fields.recipientAddress (CS.value.new fields.lovelaceAmount)
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

sendTokenToAddress :: ∀ m. Monad m => WalletApi -> SendTokenToAddressFields -> m (Maybe TxId)
sendTokenToAddress _ _ = pure Nothing
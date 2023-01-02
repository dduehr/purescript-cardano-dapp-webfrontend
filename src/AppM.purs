module Frontend.AppM where

import Prelude

import Cardano.Wallet (availableWallets, enable, getApiVersion, getChangeAddress, getIcon, getName, getUtxos, isWalletAvailable, signTx, submitTx) as CW
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Csl as CS
import Data.Array (filter)
import Data.Foldable (elem)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, sequence, traverse)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT)
import Safe.Coerce (coerce)

import Frontend.Api.WalletName (unwrap) as WalletName
import Frontend.Capability.LogMessages (class LogMessages, logHush)
import Frontend.Capability.Resource.Address (class ManageAddress) as Address
import Frontend.Capability.Resource.AuxiliaryData (class ManageAuxiliaryData, new) as AuxiliaryData
import Frontend.Capability.Resource.Contract (class ManageContract) as Contract
import Frontend.Capability.Resource.TxBuilder (class ManageTxBuilder, addChangeIfNedded, addInsFrom, addOut, build, new) as TxBuilder
import Frontend.Capability.Resource.TxWitnessSet (class ManageTxWitnessSet, new) as TxWitnessSet
import Frontend.Capability.Resource.Wallet (class ManageWallet, getChangeAddress, getTxUnspentOuts, signTx, submitTx) as Wallet
import Frontend.Capability.Resource.WebPage (class ManageWebPage, getWallet) as WebPage
import Frontend.Store as Store

newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

runAppM :: forall q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance binFrontendM :: Bind AppM
derive newtype instance monaFrontendM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Store.Action Store.Store AppM

instance logMessagesAppM :: LogMessages AppM where
  logMessage = log

instance manageWebPageAppM :: WebPage.ManageWebPage AppM where
  getWallet walletName =
    liftEffect $ do
      isWalletAvailable <- CW.isWalletAvailable walletName
      if isWalletAvailable
        then do
          apiVersion <- CW.getApiVersion walletName
          name <- CW.getName walletName
          icon <- CW.getIcon walletName
          pure $ Just
            { id: walletName
            , name: name
            , apiVersion: apiVersion
            , icon: icon
            }
        else
          pure Nothing

  availableWallets = do
    walletNames <- liftEffect $ CW.availableWallets
    blacklist <- _.blacklist <$> getStore
    sequence <$> (traverse WebPage.getWallet $ filter (\walletName -> WalletName.unwrap walletName `not elem` blacklist) walletNames)


instance manageWalletAppM :: Wallet.ManageWallet AppM where
  enableWallet walletName = do
    logHush $ liftAff (attempt $ CW.enable walletName)

  getTxUnspentOuts walletApi = do
    mbArrayCbor <- logHush $ liftAff (attempt $ CW.getUtxos walletApi Nothing)
    let mbArrayUtxo = traverse CS.txUnspentOut.fromHex =<< mbArrayCbor
    liftEffect $ for mbArrayUtxo CS.toMutableList 

  getChangeAddress walletApi = do
    mbCbor <- logHush $ liftAff (attempt $ CW.getChangeAddress walletApi)
    pure $ CS.address.fromHex =<< mbCbor

  signTx walletApi tx = do
    let cbor = CS.tx.toHex tx
    mbTxWitnessSetHex <- logHush $ liftAff (attempt $ CW.signTx walletApi cbor false)
    pure $ CS.txWitnessSet.fromHex =<< mbTxWitnessSetHex

  submitTx walletApi tx = do
    let cbor = CS.tx.toHex tx
    logHush $ liftAff (attempt $ CW.submitTx walletApi cbor)


instance manageTxBuilderAppM :: TxBuilder.ManageTxBuilder AppM where
  new = do
    mbTxBuilderConfig <- _.txBuilderConfig <$> getStore
    liftEffect $ for mbTxBuilderConfig CS.txBuilder.new

  addOut txBuilder recipientAddress lovelaceAmount = do
    let txOut = CS.txOut.new recipientAddress $ CS.value.new lovelaceAmount
    liftEffect $ CS.txBuilder.addOut txBuilder txOut

  addInsFrom txBuilder txUnspentOuts =
    liftEffect $ CS.txBuilder.addInsFrom txBuilder txUnspentOuts (toNumber 1)

  addChangeIfNedded txBuilder changeAddress =
    liftEffect $ CS.txBuilder.addChangeIfNeeded txBuilder changeAddress 

  build txBuilder =
    liftEffect $ CS.txBuilder.build txBuilder


instance manageTxWitnessSetAppM :: TxWitnessSet.ManageTxWitnessSet AppM where
  new =
    liftEffect CS.txWitnessSet.new


instance manageAuxiliaryDataAppM :: AuxiliaryData.ManageAuxiliaryData AppM where
  new =
    liftEffect CS.auxiliaryData.new


instance manageAddressAppM :: Address.ManageAddress AppM where
  sendAdaToAddress walletApi fields = runMaybeT $ do
    txBuilder <- MaybeT $ TxBuilder.new
    txUnspentOuts <- MaybeT $ Wallet.getTxUnspentOuts walletApi
    lift $ TxBuilder.addOut txBuilder fields.recipientAddress fields.lovelaceAmount
    lift $ TxBuilder.addInsFrom txBuilder txUnspentOuts
    changeAddress <- MaybeT $ Wallet.getChangeAddress walletApi
    changeAdded <- lift $ TxBuilder.addChangeIfNedded txBuilder changeAddress
    if changeAdded
      then do
        txBody <- lift $ TxBuilder.build txBuilder
        txWitnessSetEmpty <- lift $ TxWitnessSet.new
        auxiliaryDataEmpty <- lift $ AuxiliaryData.new
        let txUnsigned = CS.tx.new txBody txWitnessSetEmpty auxiliaryDataEmpty
        txWitnessSetSigned <- MaybeT $ Wallet.signTx walletApi txUnsigned
        let txSigned = CS.tx.new txBody txWitnessSetSigned auxiliaryDataEmpty
        MaybeT $ Wallet.submitTx walletApi txSigned
      else
        MaybeT $ pure Nothing


  sendTokenToAddress _ _ = do
    pure $ Nothing


instance manageContractAppM :: Contract.ManageContract AppM where
  sendAdaToContract _ _ = do
    pure $ Nothing

  sendTokenToContract _ _ = do
    pure $ Nothing

  redeemAdaFromContract _ _ = do
    pure $ Nothing

  redeemTokenFromContract _ _ = do
    pure $ Nothing
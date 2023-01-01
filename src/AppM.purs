module Example.AppM where

import Prelude

import Cardano.Wallet (availableWallets, enable, getApiVersion, getChangeAddress, getIcon, getName, getUtxos, signTx, submitTx) as CW
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Csl as Csl
import Data.Array (filter)
import Data.Either (hush)
import Data.Foldable (elem)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT)
import Safe.Coerce (coerce)

import Example.Api.WalletName (unwrap) as WalletName
import Example.Capability.Resource.Address (class ManageAddress) as Address
import Example.Capability.Resource.Contract (class ManageContract) as Contract
import Example.Capability.Resource.TxBody (class ManageTxBody, toSignedHex, toUnsignedHex) as TxBody
import Example.Capability.Resource.TxBuilder (class ManageTxBuilder, addChangeIfNedded, addInsFrom, addOut, build, new) as TxBuilder
import Example.Capability.Resource.TxWitnessSet (class ManageTxWitnessSet, fromHex) as TxWitnessSet
import Example.Capability.Resource.Wallet (class ManageWallet, getChangeAddress, getTxUnspentOuts, signTx, submitTx) as Wallet
import Example.Capability.Resource.WebPage (class ManageWebPage, getWallet) as WebPage
import Example.Store as Store

newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

runAppM :: forall q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Store.Action Store.Store AppM

instance manageWebPageAppM :: WebPage.ManageWebPage AppM where
  getWallet walletName =
    let
      mkWallet wn = do
        apiVersion <- CW.getApiVersion wn
        name <- CW.getName wn
        icon <- CW.getIcon wn
        pure $ Just
          { id: wn
          , name: name
          , apiVersion: apiVersion
          , icon: icon
          }
    in
      liftEffect $ mkWallet walletName  

  availableWallets = do
      walletNames <- liftEffect $ CW.availableWallets
      blacklist <- _.blacklist <$> getStore
      sequence <$> (traverse WebPage.getWallet $ filter (\walletName -> WalletName.unwrap walletName `not elem` blacklist) walletNames)


instance manageWalletAppM :: Wallet.ManageWallet AppM where
  enableWallet walletName = 
    hush <$> liftAff (attempt $ CW.enable walletName)

  getTxUnspentOuts walletApi = do
    mbArrayCbor <- hush <$> liftAff (attempt $ CW.getUtxos walletApi Nothing)
    let mbArrayUtxo = mbArrayCbor >>= traverse Csl.txUnspentOut.fromHex
    liftEffect $ traverse Csl.toMutableList mbArrayUtxo  

  getChangeAddress walletApi = do
    mbCbor <- hush <$> liftAff (attempt $ CW.getChangeAddress walletApi)
    pure $ mbCbor >>= Csl.address.fromHex

  signTx walletApi txBody = runMaybeT $ do
    txUnsignedHex <- lift $ TxBody.toUnsignedHex txBody
    txWitnessSetHex <- MaybeT $ hush <$> liftAff (attempt $ CW.signTx walletApi txUnsignedHex false)
    MaybeT $ TxWitnessSet.fromHex txWitnessSetHex

  submitTx walletApi txBody txWitnessSet = runMaybeT $ do
    txSignedHex <- lift $ TxBody.toSignedHex txBody txWitnessSet
    MaybeT $ hush <$> liftAff (attempt $ CW.submitTx walletApi txSignedHex)

instance manageTxBuilderAppM :: TxBuilder.ManageTxBuilder AppM where
  new = do
    mbTxBuilderConfig <- _.txBuilderConfig <$> getStore
    liftEffect $ traverse Csl.txBuilder.new mbTxBuilderConfig

  addOut txBuilder recipientAddress lovelaceAmount = do
    let txOut = Csl.txOut.new recipientAddress $ Csl.value.new lovelaceAmount
    liftEffect $ Csl.txBuilder.addOut txBuilder txOut

  addInsFrom txBuilder txUnspentOuts =
    liftEffect $ Csl.txBuilder.addInsFrom txBuilder txUnspentOuts $ toNumber 1

  addChangeIfNedded txBuilder changeAddress =
    liftEffect $ Csl.txBuilder.addChangeIfNeeded txBuilder changeAddress 

  build txBuilder =
    liftEffect $ Csl.txBuilder.build txBuilder


instance manageTxBodyAppM :: TxBody.ManageTxBody AppM where
  toUnsignedHex txBody = do
    txWitnessSetEmpty <- liftEffect $ Csl.txWitnessSet.new
    auxiliaryDataEmpty <- liftEffect $ Csl.auxiliaryData.new
    pure $ Csl.tx.toHex $ Csl.tx.new txBody txWitnessSetEmpty auxiliaryDataEmpty

  toSignedHex txBody txWitnessSet = do
    auxiliaryDataEmpty <- liftEffect $ Csl.auxiliaryData.new
    pure $ Csl.tx.toHex $ Csl.tx.new txBody txWitnessSet auxiliaryDataEmpty


instance manageTxWitnessSet :: TxWitnessSet.ManageTxWitnessSet AppM where
  fromHex = pure <<< Csl.txWitnessSet.fromHex


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
        txWitnessSet <- MaybeT $ Wallet.signTx walletApi txBody
        MaybeT $ Wallet.submitTx walletApi txBody txWitnessSet
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
module Frontend.AppM where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Csl as CS
import Data.Array.NonEmpty (fromArray, toArray)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (sequence, traverse)
import Effect.Class.Console (log)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT)
import Safe.Coerce (coerce)

import Frontend.Api.AuxiliaryData (new) as AuxiliaryData
import Frontend.Api.Browser (availableWallets, enable, disable, getApiVersion, getIcon, getName, isWalletAvailable)
import Frontend.Api.TxBuilder (addChangeIfNeeded, addOut, addInsFrom, build, new) as TxBuilder
import Frontend.Api.WalletApi (getBalance, getChangeAddress, getNetworkId, getRewardAddresses, getUsedAddresses, getUtxos, signTx, submitTx)
import Frontend.Api.TxUnspentOuts (new) as TxUnspentOuts
import Frontend.Api.TxWitnessSet (new) as TxWitnessSet
import Frontend.Capability.LogMessages (class LogMessages)
import Frontend.Capability.Resource.Address (class ManageAddress) as Address
import Frontend.Capability.Resource.Browser (class ManageBrowser, getWallet) as Browser
import Frontend.Capability.Resource.Contract (class ManageContract) as Contract
import Frontend.Capability.Resource.Wallet (class ManageWallet, getChangeAddress, getUtxos, signTx, submitTx) as Wallet
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

instance manageBrowserAppM :: Browser.ManageBrowser AppM where
  getWallets = runMaybeT $ do
    nonEmptyIds <- MaybeT $ availableWallets
    wallets <- MaybeT $ sequence <$> (traverse Browser.getWallet $ toArray nonEmptyIds)
    MaybeT $ pure $ fromArray wallets

  getWallet id = runMaybeT $ do
    isWalletAvailable <- MaybeT $ isWalletAvailable id
    if isWalletAvailable then do
      name <- MaybeT $ getName id
      apiVersion <- MaybeT $ getApiVersion id
      icon <- MaybeT $ getIcon id
      pure { id, name, apiVersion, icon }
    else
      MaybeT $ pure Nothing

  enableWallet id = do
    mbApi <- enable id
    pure $ isJust mbApi

  disableWallet =
    disable

instance manageWalletAppM :: Wallet.ManageWallet AppM where
  getNetworkId =
    getNetworkId

  getBalance api = do
    mbHex <- getBalance api
    pure $ CS.bigNum.fromHex =<< mbHex

  getChangeAddress api = do
    mbHex <- getChangeAddress api
    pure $ CS.address.fromHex =<< mbHex

  getRewardAddresses api = do
    mbHex <- getRewardAddresses api
    pure $ join $ traverse CS.address.fromHex <$> mbHex

  getUsedAddresses api = do
    mbHex <- getUsedAddresses api { limit: 10, page: 0 }
    pure $ join $ traverse CS.address.fromHex <$> mbHex

  getUtxos api = do
    mbHex <- getUtxos api Nothing
    pure $ join $ traverse CS.txUnspentOut.fromHex <$> mbHex

  signTx api tx = do
    mbHex <- signTx api (CS.tx.toHex tx) false
    pure $ CS.txWitnessSet.fromHex =<< mbHex

  submitTx api tx =
    submitTx api $ CS.tx.toHex tx

instance manageAddressAppM :: Address.ManageAddress AppM where
  sendAdaToAddress api fields = runMaybeT $ do
    txBuilderConfig <- MaybeT $ _.mbTxBuilderConfig <$> getStore
    txBuilder <- MaybeT $ TxBuilder.new txBuilderConfig
    txUnspentOutArray <- MaybeT $ Wallet.getUtxos api
    txUnspentOuts <- MaybeT $ TxUnspentOuts.new txUnspentOutArray
    let txOut = CS.txOut.new fields.recipientAddress (CS.value.new fields.lovelaceAmount)
    MaybeT $ TxBuilder.addOut txBuilder txOut
    MaybeT $ TxBuilder.addInsFrom txBuilder txUnspentOuts (toNumber 1)
    changeAddress <- MaybeT $ Wallet.getChangeAddress api
    changeAdded <- MaybeT $ TxBuilder.addChangeIfNeeded txBuilder changeAddress
    if changeAdded then do
      txBody <- MaybeT $ TxBuilder.build txBuilder
      txWitnessSetEmpty <- MaybeT $ TxWitnessSet.new
      auxiliaryDataEmpty <- MaybeT $ AuxiliaryData.new
      let txUnsigned = CS.tx.new txBody txWitnessSetEmpty auxiliaryDataEmpty
      txWitnessSetSigned <- MaybeT $ Wallet.signTx api txUnsigned
      let txSigned = CS.tx.new txBody txWitnessSetSigned auxiliaryDataEmpty
      MaybeT $ Wallet.submitTx api txSigned
    else
      MaybeT $ pure Nothing

  sendTokenToAddress _ _ = pure Nothing

instance manageContractAppM :: Contract.ManageContract AppM where
  sendAdaToContract _ _ = pure Nothing
  sendTokenToContract _ _ = pure Nothing
  redeemAdaFromContract _ _ = pure Nothing
  redeemTokenFromContract _ _ = pure Nothing

instance logMessagesAppM :: LogMessages AppM where
  logMessage = log
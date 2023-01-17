module Frontend.AppM where

import Prelude

import Effect.Class.Console (log)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT)
import Safe.Coerce (coerce)

import Frontend.Api.Domain.Address (class ManageAddress) as Domain
import Frontend.Api.Domain.Browser (class ManageBrowser) as Domain
import Frontend.Api.Domain.Contract (class ManageContract) as Domain
import Frontend.Api.Domain.Wallet (class ManageWallet) as Domain

import Frontend.Capability.Domain.Address (sendAdaToAddress, sendTokenToAddress) as Domain
import Frontend.Capability.Domain.Browser (disableWallet, enableWallet, getWallet, getWallets) as Domain
import Frontend.Capability.Domain.Contract (redeemAdaFromContract, redeemTokenFromContract, sendAdaToContract, sendTokenToContract) as Domain
import Frontend.Capability.Domain.Wallet (getWalletBalance, getWalletChangeAddress, getWalletNetworkId, getWalletRewardAddresses, getWalletUsedAddresses, getWalletUtxos, signTx, submitTx) as Domain

import Frontend.Api.Infrastructure.Cip30.Browser (class ManageBrowser) as Infrastructure
import Frontend.Api.Infrastructure.Cip30.WalletApi (class ManageWalletApi) as Infrastructure
import Frontend.Api.Infrastructure.Csl.Serialization (class ManageSerialization) as Infrastructure

import Frontend.Capability.Infrastructure.Cip30.Browser (enableWalletApi, getAvailableWallets, getWalletApiVersion, getWalletIcon, getWalletName, isWalletAvailable) as Infrastructure
import Frontend.Capability.Infrastructure.Cip30.WalletApi (getWalletBalance, getWalletChangeAddress, getWalletNetworkId, getWalletRewardAddresses, getWalletUsedAddresses, getWalletUtxos, signTx, submitTx) as Infrastructure
import Frontend.Capability.Infrastructure.Csl.Serialization (newAuxiliaryData, newTxBuilder, newTxUnspentOuts, newWitnessSet, txBuilderAddChangeIfNeeded, txBuilderAddInsFrom, txBuilderAddOut, txBuilderBuild, txOutSetDataHash) as Infrastructure

import Frontend.Api.LogMessages (class LogMessages)
import Frontend.Store (Action, Store, reduce) as Store

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

instance domainManageBrowserAppM :: Domain.ManageBrowser AppM where
  getWallets = Domain.getWallets
  getWallet = Domain.getWallet
  enableWallet = Domain.enableWallet
  disableWallet = Domain.disableWallet

instance domainManageWalletAppM :: Domain.ManageWallet AppM where
  getWalletNetworkId = Domain.getWalletNetworkId
  getWalletBalance = Domain.getWalletBalance
  getWalletChangeAddress = Domain.getWalletChangeAddress
  getWalletRewardAddresses = Domain.getWalletRewardAddresses
  getWalletUsedAddresses = Domain.getWalletUsedAddresses
  getWalletUtxos = Domain.getWalletUtxos
  signTx = Domain.signTx
  submitTx = Domain.submitTx

instance domainManageAddressAppM :: Domain.ManageAddress AppM where
  sendAdaToAddress = Domain.sendAdaToAddress
  sendTokenToAddress = Domain.sendTokenToAddress

instance domainManageContractAppM :: Domain.ManageContract AppM where
  sendAdaToContract = Domain.sendAdaToContract
  sendTokenToContract = Domain.sendTokenToContract
  redeemAdaFromContract = Domain.redeemAdaFromContract
  redeemTokenFromContract = Domain.redeemTokenFromContract

instance infrastructureManageBrowserAppM :: Infrastructure.ManageBrowser AppM where
  getAvailableWallets = Infrastructure.getAvailableWallets
  isWalletAvailable = Infrastructure.isWalletAvailable
  getWalletApiVersion = Infrastructure.getWalletApiVersion
  getWalletIcon = Infrastructure.getWalletIcon
  getWalletName = Infrastructure.getWalletName
  enableWalletApi = Infrastructure.enableWalletApi

instance infrastructureManageWalletApi :: Infrastructure.ManageWalletApi AppM where
  getWalletBalance = Infrastructure.getWalletBalance
  getWalletChangeAddress = Infrastructure.getWalletChangeAddress
  getWalletNetworkId = Infrastructure.getWalletNetworkId
  getWalletRewardAddresses = Infrastructure.getWalletRewardAddresses
  getWalletUsedAddresses = Infrastructure.getWalletUsedAddresses
  getWalletUtxos = Infrastructure.getWalletUtxos
  signTx = Infrastructure.signTx
  submitTx = Infrastructure.submitTx

instance infrastructureManageSerialization :: Infrastructure.ManageSerialization AppM where
  newAuxiliaryData = Infrastructure.newAuxiliaryData
  newTxUnspentOuts = Infrastructure.newTxUnspentOuts
  newWitnessSet = Infrastructure.newWitnessSet
  newTxBuilder = Infrastructure.newTxBuilder
  txBuilderAddChangeIfNeeded = Infrastructure.txBuilderAddChangeIfNeeded
  txBuilderAddInsFrom = Infrastructure.txBuilderAddInsFrom
  txBuilderAddOut = Infrastructure.txBuilderAddOut
  txBuilderBuild = Infrastructure.txBuilderBuild
  txOutSetDataHash = Infrastructure.txOutSetDataHash

instance logMessagesAppM :: LogMessages AppM where
  logMessage = log
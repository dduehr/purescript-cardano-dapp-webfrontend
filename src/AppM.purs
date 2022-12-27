module Example.AppM where

import Prelude

import Cardano.Wallet (availableWallets, enable, getApiVersion, getChangeAddress, getIcon, getName, getUtxos, signTx, submitTx) as CW
import Control.Monad.Error.Class (try)
import Control.Monad.Except.Trans (throwError)
import Csl (TxUnspentOuts, address, auxiliaryData, toMutableList, tx, txBuilder, txOut, txUnspentOut, txWitnessSet, value) as Csl
import Data.Array (filter, intercalate)
import Data.Either (Either(..), note)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT)
import Safe.Coerce (coerce)

import Example.Api.WalletName (unwrap) as WalletName
import Example.Capability.Resource.Address (class ManageAddress)
import Example.Capability.Resource.Contract (class ManageContract) 
import Example.Capability.Resource.Wallet (class ManageWallet)
import Example.Capability.Resource.WebPage (class ManageWebPage)
import Example.Component.Utils (exceptAff)
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

type Error = String
type TxId  = String
instance manageWebPageAppM :: ManageWebPage AppM where
  availableWallets =
    let
      mkWallet walletName = do
        apiVersion <- CW.getApiVersion walletName
        name <- CW.getName walletName
        icon <- CW.getIcon walletName
        pure
          { id: walletName
          , name: name
          , apiVersion: apiVersion
          , icon: icon
          }
    in do
      walletNames <- H.liftEffect $ CW.availableWallets
      log $ "Wallets found: " <> intercalate ", " (WalletName.unwrap <$> walletNames)
      wallets <- H.liftEffect $ traverse mkWallet $ filter (\walletName -> WalletName.unwrap walletName /= "ccvault") walletNames
      pure $ Just wallets


instance manageWalletAppM :: ManageWallet AppM where
  enableWallet walletName = do
    result <- liftAff $ try $ CW.enable walletName
    case result of
      Right api -> pure api
      Left error -> throwError $ show error 
  
instance manageAddressAppM :: ManageAddress AppM where
  sendAdaToAddress walletApi txBuilderConfig fields = do
    txUnspentOutArray <- exceptAff $ note "Failed to get UTxOs" <$> traverse Csl.txUnspentOut.fromHex <$> CW.getUtxos walletApi Nothing 
    txUnspentOuts <- liftEffect (Csl.toMutableList txUnspentOutArray :: Effect Csl.TxUnspentOuts)
    changeAddress <- exceptAff $ note "Failed to get change address" <$> Csl.address.fromHex <$> CW.getChangeAddress walletApi 
    txBuilder <- liftEffect $ Csl.txBuilder.new txBuilderConfig
    liftEffect $ Csl.txBuilder.addOut txBuilder $ Csl.txOut.new fields.recipientAddress $ Csl.value.new fields.lovelaceAmount
    liftEffect $ Csl.txBuilder.addInsFrom txBuilder txUnspentOuts $ toNumber 1
    -- Fixme: addChangeIfNedded might return <false>
    _ <- liftEffect $ Csl.txBuilder.addChangeIfNeeded txBuilder changeAddress
    txBody <- liftEffect $ Csl.txBuilder.build txBuilder
    txWitnessSetEmpty <- liftEffect $ Csl.txWitnessSet.new
    auxiliaryDataEmpty <- liftEffect $ Csl.auxiliaryData.new
    let txUnsignedHex = Csl.tx.toHex $ Csl.tx.new txBody txWitnessSetEmpty auxiliaryDataEmpty
    txWitnessSet <- exceptAff $ note "Failed to sign" <$> Csl.txWitnessSet.fromHex <$> CW.signTx walletApi txUnsignedHex false
    let txSignedHex = Csl.tx.toHex $ Csl.tx.new txBody txWitnessSet auxiliaryDataEmpty
    log $ "txSignedHex: " <> txSignedHex
    result <- liftAff $ try $ CW.submitTx walletApi txSignedHex
    case result of
      Right txId -> pure txId
      Left error -> throwError $ show error

  sendTokenToAddress _ _ _ = do
    throwError "Not implemented"


instance manageContractAppM :: ManageContract AppM where
  sendAdaToContract _ _ _ = do
    throwError "Not implemented"

  sendTokenToContract _ _ _ = do
    throwError "Not implemented"

  redeemAdaFromContract _ _ _ = do
    throwError "Not implemented"

  redeemTokenFromContract _ _ _ = do
    throwError "Not implemented"
module Qapla.AppM
  where

import Prelude

import Cardano.Wallet (getChangeAddress, getUtxos, signTx, submitTx) as CW
import Control.Monad.Error.Class (try)
import Control.Monad.Except.Trans (ExceptT(..), throwError)
import Control.Monad.Reader.Class (class MonadAsk, asks)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Csl (TxUnspentOuts, TxBuilderConfig, address, auxiliaryData, toMutableList, tx, txBuilder, txOut, txUnspentOut, txWitnessSet, value) as Csl
import Data.Either (Either(..), note)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Qapla.Capability.Resource.Address (class ManageAddress)
import Type.Equality (class TypeEquals, from) as TE

type Env = { txBuilderConfig :: Maybe Csl.TxBuilderConfig }

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TE.TypeEquals env Env => MonadAsk env AppM where
  ask = AppM $ asks TE.from


type Error = String
type TxId  = String

exceptAff :: ∀ a m. MonadAff m => Aff (Either Error a) -> ExceptT Error m a    
exceptAff = ExceptT <<< liftAff

exceptM :: ∀ a m. Monad m => m (Either Error a) -> ExceptT Error m a
exceptM = ExceptT

instance manageAddressAppM :: ManageAddress AppM where
  sendAdaToAddress wallet fields = do
    txUnspentOutArray <- exceptAff $ note "Failed to get UTxOs" <$> traverse Csl.txUnspentOut.fromHex <$> CW.getUtxos wallet Nothing 
    txUnspentOuts <- liftEffect (Csl.toMutableList txUnspentOutArray :: Effect Csl.TxUnspentOuts)
    changeAddress <- exceptAff $ note "Failed to get change address" <$> Csl.address.fromHex <$> CW.getChangeAddress wallet 
    txBuilderConfig <- exceptM $ note "Failed to get transaction builder configuration" <$> asks _.txBuilderConfig
    txBuilder <- liftEffect $ Csl.txBuilder.new txBuilderConfig
    liftEffect $ Csl.txBuilder.addOut txBuilder $ Csl.txOut.new fields.recipientAddress $ Csl.value.new fields.lovelaceAmount
    liftEffect $ Csl.txBuilder.addInsFrom txBuilder txUnspentOuts $ toNumber 1
    -- Fixme: addChangeIfNedded might return <false>
    _ <- liftEffect $ Csl.txBuilder.addChangeIfNeeded txBuilder changeAddress
    txBody <- liftEffect $ Csl.txBuilder.build txBuilder
    txWitnessSetEmpty <- liftEffect $ Csl.txWitnessSet.new
    auxiliaryDataEmpty <- liftEffect $ Csl.auxiliaryData.new
    let txUnsignedHex = Csl.tx.toHex $ Csl.tx.new txBody txWitnessSetEmpty auxiliaryDataEmpty
    txWitnessSet <- exceptAff $ note "Failed to sign" <$> Csl.txWitnessSet.fromHex <$> CW.signTx wallet txUnsignedHex false
    let txSignedHex = Csl.tx.toHex $ Csl.tx.new txBody txWitnessSet auxiliaryDataEmpty
    log $ "txSignedHex: " <> txSignedHex
    result <- liftAff $ try $ CW.submitTx wallet txSignedHex
    case result of
      Right txId -> pure txId
      Left error -> throwError $ show error


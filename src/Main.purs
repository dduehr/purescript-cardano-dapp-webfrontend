module Main (main) where

import Prelude (Unit, (<>), bind, const, discard, flip, pure, show, unit, ($), (<$>))

import Cardano.Wallet (Api)
import Cardano.Wallet (getChangeAddress, getUtxos, signTx, submitTx) as CW
import Control.Monad.Error.Class (try)
import Control.Monad.Except.Trans (ExceptT(..), except, runExceptT, throwError)
import Csl as Csl
import Data.Either (Either(..), note)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))

import EnableWallet (Output(..), component) as EnableWallet
import SendAdaToAddressForm (Output, form) as SendAdaToAddressForm

type State = Maybe Api

data Action
  = HandleEnableWallet EnableWallet.Output
  | HandleSendAdaToAddressForm SendAdaToAddressForm.Output

type Slots =
  ( wallet :: ∀ query. H.Slot query EnableWallet.Output Unit
  , form :: ∀ query. H.Slot query SendAdaToAddressForm.Output Unit
  )

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: ∀ query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: const Nothing
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: ∀ state m. MonadAff m => state -> H.ComponentHTML Action Slots m
render _ =
  HH.div_
    [ HH.slot (Proxy :: _ "wallet") unit EnableWallet.component unit HandleEnableWallet
    , HH.slot (Proxy :: _ "form") unit SendAdaToAddressForm.form unit HandleSendAdaToAddressForm
    ]

handleAction :: ∀ output m. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  HandleEnableWallet (EnableWallet.WalletEnabled api) -> do
    H.put $ Just api
    log "Wallet API available"
  HandleSendAdaToAddressForm output -> do
    state <- H.get
    case state of
      (Just api) -> do
        result <- runExceptT $ sendAdaToAddress api output.recipient output.amount
        case result of
          (Left error) -> log error
          (Right txId) -> log $ "Transaction submitted with id: " <> txId
      _ -> log "No wallet connected"


exceptAff :: ∀ a m. MonadAff m => Aff (Either String a) -> ExceptT String m a    
exceptAff action = ExceptT $ liftAff action

sendAdaToAddress :: ∀ m. MonadAff m => Api -> Csl.Address -> Csl.BigNum -> ExceptT String m String
sendAdaToAddress api recipient amount = do
  txUnspentOutArray <- exceptAff $ note "Failed to get UTxOs" <$> traverse Csl.txUnspentOut.fromHex <$> CW.getUtxos api Nothing 
  txUnspentOuts <- liftEffect (Csl.toMutableList txUnspentOutArray :: Effect Csl.TxUnspentOuts)
  changeAddress <- exceptAff $ note "Failed to get change address" <$> Csl.address.fromHex <$> CW.getChangeAddress api 
  txBuilderConfig <- except $ note "Failed to create TxBuilderConfig" mkTxBuilderConfig
  txBuilder <- liftEffect $ Csl.txBuilder.new txBuilderConfig
  liftEffect $ Csl.txBuilder.addOut txBuilder $ Csl.txOut.new recipient $ Csl.value.new amount
  liftEffect $ Csl.txBuilder.addInsFrom txBuilder txUnspentOuts $ toNumber 1
  -- Fixme: addChangeIfNedded might return <false>
  _ <- liftEffect $ Csl.txBuilder.addChangeIfNeeded txBuilder changeAddress
  txBody <- liftEffect $ Csl.txBuilder.build txBuilder
  txWitnessSetEmpty <- liftEffect $ Csl.txWitnessSet.new
  auxiliaryDataEmpty <- liftEffect $ Csl.auxiliaryData.new
  let txUnsignedHex = Csl.tx.toHex $ Csl.tx.new txBody txWitnessSetEmpty auxiliaryDataEmpty
  txWitnessSet <- exceptAff $ note "Failed to sign" <$> Csl.txWitnessSet.fromHex <$> CW.signTx api txUnsignedHex false
  let txSignedHex = Csl.tx.toHex $ Csl.tx.new txBody txWitnessSet auxiliaryDataEmpty
  log $ "txSignedHex: " <> txSignedHex
  result <- liftAff $ try $ CW.submitTx api txSignedHex
  case result of
    Right txId -> pure txId
    Left error -> throwError $ show error

mkTxBuilderConfig :: Maybe Csl.TxBuilderConfig
mkTxBuilderConfig = do
  feeCoefficient <- Csl.bigNum.fromStr "44"
  feeConstant <- Csl.bigNum.fromStr "155381"
  poolDeposit <- Csl.bigNum.fromStr "500000000"
  keyDeposit <- Csl.bigNum.fromStr "2000000"
  coinsPerUtxoWord <- Csl.bigNum.fromStr "34482"
  pure $ Csl.txBuilderConfigBuilder.build
    $ flip Csl.txBuilderConfigBuilder.preferPureChange true
    $ flip Csl.txBuilderConfigBuilder.maxTxSize 16384
    $ flip Csl.txBuilderConfigBuilder.maxValueSize 5000
    $ flip Csl.txBuilderConfigBuilder.coinsPerUtxoWord coinsPerUtxoWord
    $ flip Csl.txBuilderConfigBuilder.keyDeposit keyDeposit
    $ flip Csl.txBuilderConfigBuilder.poolDeposit poolDeposit
    $ flip Csl.txBuilderConfigBuilder.feeAlgo (Csl.linearFee.new feeCoefficient feeConstant)
    $ Csl.txBuilderConfigBuilder.new 
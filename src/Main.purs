module Main (main) where

import Prelude

import Csl (TxBuilderConfig, bigNum, linearFee, txBuilderConfigBuilder) as CS
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..))
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Frontend.AppM (runAppM)
import Frontend.Page.Root (component) as Root
import Frontend.Store (Store) as Store

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  rootComponent <- runAppM initialStore Root.component
  runUI rootComponent unit body
  where

  initialStore :: Store.Store
  initialStore =
    { mbWalletCredentials: Nothing
    , mbWalletReloadSec: Just $ Seconds (toNumber 60)
    , mbTxBuilderConfig
    , blacklistedWallets: [ "ccvault" ]
    }

  mbTxBuilderConfig :: Maybe CS.TxBuilderConfig
  mbTxBuilderConfig = do
    feeCoefficient <- CS.bigNum.fromStr "44"
    feeConstant <- CS.bigNum.fromStr "155381"
    poolDeposit <- CS.bigNum.fromStr "500000000"
    keyDeposit <- CS.bigNum.fromStr "2000000"
    coinsPerUtxoWord <- CS.bigNum.fromStr "34482"
    pure $ CS.txBuilderConfigBuilder.build
      $ flip CS.txBuilderConfigBuilder.preferPureChange true
      $ flip CS.txBuilderConfigBuilder.maxTxSize 16384
      $ flip CS.txBuilderConfigBuilder.maxValueSize 5000
      $ flip CS.txBuilderConfigBuilder.coinsPerUtxoWord coinsPerUtxoWord
      $ flip CS.txBuilderConfigBuilder.keyDeposit keyDeposit
      $ flip CS.txBuilderConfigBuilder.poolDeposit poolDeposit
      $ flip CS.txBuilderConfigBuilder.feeAlgo (CS.linearFee.new feeCoefficient feeConstant)
      $ CS.txBuilderConfigBuilder.new
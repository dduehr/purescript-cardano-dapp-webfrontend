module Main (main) where

import Prelude

import Csl (TxBuilderConfig, bigNum, linearFee, txBuilderConfigBuilder) as Csl
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Frontend.AppM (runAppM)
import Frontend.Page.Root (component) as Root

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let initialStore = { blacklist, wallet: Nothing, txBuilderConfig }
  rootComponent <- runAppM initialStore Root.component
  runUI rootComponent unit body
  where

    blacklist :: Array String
    blacklist = [ "ccvault" ]  

    txBuilderConfig :: Maybe Csl.TxBuilderConfig
    txBuilderConfig = do
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
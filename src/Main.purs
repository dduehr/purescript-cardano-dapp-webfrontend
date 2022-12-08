module Main (main) where

import Prelude

import Csl (TxBuilderConfig, bigNum, linearFee, txBuilderConfigBuilder) as Csl
import Data.Maybe (Maybe)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Qapla.AppM (runAppM)
import Qapla.Page.Root (component) as Root

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let
    env  = { txBuilderConfig: mkTxBuilderConfig }
    root = H.hoist (runAppM env) Root.component
  runUI root unit body

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
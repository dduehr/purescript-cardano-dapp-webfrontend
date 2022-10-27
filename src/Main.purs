module Main (main) where

import Prelude (Unit, ($), bind, const, discard, unit)

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))

import Cardano.Wallet (Api)

import EnableWallet (Output(..), component) as EnableWallet

type State = Maybe Api

data Action = HandleEnableWallet EnableWallet.Output

type Slots = ( wallet :: forall query. H.Slot query EnableWallet.Output Unit )

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: const Nothing
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall state m. MonadAff m => state -> H.ComponentHTML Action Slots m
render _ =
  HH.div_
    [ HH.slot (Proxy :: _ "wallet") unit EnableWallet.component unit HandleEnableWallet ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  HandleEnableWallet (EnableWallet.WalletEnabled api) -> do
    H.put $ Just api
    log "wallet api available"
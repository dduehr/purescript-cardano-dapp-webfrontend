module Main (main) where

import Prelude (Unit, Void, bind, identity, unit)

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Effect.Aff.Class (class MonadAff)

import Wallets (component) as Wallets

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type Slots = ( wallets :: forall query. H.Slot query Void Unit )

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }

render :: forall state action m. MonadAff m => state -> H.ComponentHTML action Slots m
render _ =
  HH.div_
    [ HH.slot_ (Proxy :: _ "wallets") unit Wallets.component unit ]
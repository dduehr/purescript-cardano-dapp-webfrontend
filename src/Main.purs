module Main (main) where

import Prelude (Unit, ($), (<>), bind, identity, unit)

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Effect.Aff.Class (class MonadAff)

import Wallets (Output(..), component, tag) as Wallets

data Action = HandleWallets Wallets.Output

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type Slots = ( wallets :: forall query. H.Slot query Wallets.Output Unit )

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall state m. MonadAff m => state -> H.ComponentHTML Action Slots m
render _ =
  HH.div_
    [ HH.slot (Proxy :: _ "wallets") unit Wallets.component unit HandleWallets ]

handleAction :: forall output state m. MonadEffect m => Action -> H.HalogenM state Action Slots output m Unit
handleAction = case _ of
  HandleWallets (Wallets.WalletSelected walletName) -> do
    log $ "From parent: TODO enable wallet " <> Wallets.tag walletName
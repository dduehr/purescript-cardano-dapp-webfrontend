module Main (main) where

import Prelude (Unit, bind, const, discard, pure, show, unit, ($), (<>))

import Cardano.Wallet (Api)
import Csl as Csl
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
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
    log "wallet api available"
  HandleSendAdaToAddressForm output -> do
    state <- H.get
    case state of
      (Just api) -> H.liftAff $ sendAdaToAddress api output.recipient output.amount
      _ -> log "No wallet connected"

sendAdaToAddress :: ∀ m. MonadAff m => Api -> Csl.Address -> Csl.BigNum -> m Unit
sendAdaToAddress api recipient amount = do
  log $ "TODO: send " <> show amount <> " Lovelace to " <> show recipient
  pure unit


module Main where

import Prelude

import Data.Maybe(Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)

import Cardano.Wallet (Api)
import Cardano.Wallet (enable, flint, apiVersion, name, icon) as Wallet
import Csl

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type Wallet = 
  { api :: Api
  , name :: String
  , version :: String
  , icon :: String
  }

type State = Maybe Wallet

data Action = Initialize | EnableWallet

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
    }

initialState :: forall input. input -> State
initialState _ = Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render Nothing =
  HH.div_
    [ HH.text "No wallet" ]
render (Just wallet) =
  HH.div_
    [ HH.text wallet.name
    , HH.img [ HP.src wallet.icon, HP.width 24, HP.height 24, HP.alt wallet.name ]
    , HH.text wallet.version
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.fork $ delayAction EnableWallet $ Milliseconds 1000.0 
    pure unit

  EnableWallet -> do
    let walletName = Wallet.flint
    api <- H.liftAff $ Wallet.enable walletName
    version <- H.liftEffect $ Wallet.apiVersion walletName
    name <- H.liftEffect $ Wallet.name walletName
    icon <- H.liftEffect $ Wallet.icon walletName
    _ <- H.modify \_ -> Just { api: api, version: version, name: name, icon: icon }
    pure unit

delayAction :: forall output m. MonadAff m => Action -> Milliseconds -> H.HalogenM State Action () output m Unit
delayAction action ms = do
    H.liftAff $ Aff.delay ms
    handleAction action

module Qapla.Component.SendAdaToAddress (component) where

import Prelude (Unit, ($), (<<<))

import Cardano.Wallet (Api)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Qapla.Component.Utils (css)

data Action = Receive Input

type Input = Maybe Api

type State = Maybe Api

component :: ∀ query output m. MonadAff m => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }

initialState :: Input -> State
initialState input = input

render :: ∀ m. MonadAff m => State -> H.ComponentHTML Action () m
render _ = 
  HH.form_
    [ HH.div [ css "field" ]
      [ HH.label [ css "label" ]
        [ HH.text "Address where to send ADA" ]
      , HH.div [ css "control has-icons-left" ]
        [ HH.input [ css "input", HP.type_ HP.InputText, HP.placeholder "e.g. addr_test1qrt...", HP.required true ]
        , HH.span [ css "icon is-small is-left" ]
          [ HH.i [ css "fa fa-solid fa-address-card" ] [] ]
        ]
      ]
    , HH.div [ css "field" ]
      [ HH.label [ css "label" ]
        [ HH.text "Lovelaces (1 000 000 Lovelace = 1 ADA)" ]
      , HH.div [ css "control has-icons-left" ]
        [ HH.input [ css "input", HP.type_ HP.InputText, HP.placeholder "e.g. 1000000", HP.required true ]
        , HH.span [ css "icon is-small is-left" ]
          [ HH.i [ css "fa fa-solid fa-coins" ] [] ]
        ]
      ]
    , HH.div [ css "field" ]
      [ HH.button [ css "button is-medium is-success" ] 
        [ HH.text "Submit" ]
      ]
    ]

handleAction :: ∀ output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction (Receive input) = H.put input
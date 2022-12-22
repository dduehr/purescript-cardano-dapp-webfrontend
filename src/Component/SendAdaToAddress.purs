module Example.Component.SendAdaToAddress (component) where

import Prelude (Unit, ($), (<<<))

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectAll)

import Example.Capability.Resource.Address (class ManageAddress)
import Example.Component.Utils (css)
import Example.Store (Action, Store) as Store

type Input = Unit

type State = Store.Store

data Action = 
  Receive (Connected Store.Store Input)

component
  :: ∀ query output m
   . MonadStore Store.Action Store.Store m
  => ManageAddress m
  => H.Component query Input output m
component =
  connect selectAll $ H.mkComponent
    { initialState: deriveState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }
  where

    deriveState :: Connected Store.Store Input -> State
    deriveState { context } = context

    render :: State -> H.ComponentHTML Action () m
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

    handleAction :: Action -> H.HalogenM State Action () output m Unit
    handleAction (Receive input) = H.put $ deriveState input
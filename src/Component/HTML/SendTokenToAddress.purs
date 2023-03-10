module Frontend.Component.HTML.SendTokenToAddress (component) where

import Prelude (Unit, ($), (<<<))

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectAll)

import Frontend.Api.Domain.Address (class ManageAddress)
import Frontend.Data.Tx (TxId)
import Frontend.Store (Action, Store) as Store

type Input = Unit

type Output = Maybe TxId

type State = Store.Store

data Action =
  Receive (Connected Store.Store Input)

component
  :: ∀ query m
   . MonadStore Store.Action Store.Store m
  => ManageAddress m
  => H.Component query Input Output m
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
    HH.form_ -- TODO: disabled if (isNothing state)
      [ HH.text "TODO: SendTokenToAddress…"
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction (Receive input) = H.put $ deriveState input
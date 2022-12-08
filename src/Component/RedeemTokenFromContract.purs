module Qapla.Component.RedeemTokenFromContract (component) where

import Prelude (Unit, ($), (<<<))

import Cardano.Wallet (Api)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

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
  HH.form_ -- TODO: disabled if (isNothing state)
    [
      HH.text "TODO: RedeemTokenFromContract ..."
    ]

handleAction :: ∀ output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction (Receive input) = H.put input

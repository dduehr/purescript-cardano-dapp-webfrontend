module Frondend.Component.LoadText where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

import Frontend.Capability.Domain.Wallet (class ManageWallet)
import Frontend.Component.HTML.Utils (spinner, whenElem)

type Input = Unit

type State =
  { text :: Maybe String
  , loading :: Boolean
  }

data Action = Reload

component
  :: ∀ query output m
   . ManageWallet m
  => H.HalogenM State Action () output m (Maybe String)
  -> H.Component query Input output m
component load =
  H.mkComponent
    { initialState: const { text: Nothing, loading: false }
    , render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Reload
        , handleAction = handleAction
        , receive = const $ Just Reload
        }
    }
  where

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Reload -> do
      H.modify_ \state -> state { loading = true }
      text <- load
      H.modify_ \state -> state { loading = false, text = text }
      pure unit

  render :: State -> H.ComponentHTML Action () m
  render { text: Just text, loading } =
    HH.div_
      [ HH.text text
      , HH.text " "
      , whenElem loading $ const spinner
      ]

  render { loading } =
    whenElem loading $ const spinner
module Frontend.Component.LoadTexts where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, head, toArray)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

import Frontend.Api.Domain.Wallet (class ManageWallet)
import Frontend.Component.HTML.Utils (css, spinner, whenElem)

type Input = Unit

type State =
  { texts :: Maybe (NonEmptyArray String)
  , loading :: Boolean
  }

data Action = Reload

component
  :: ∀ query output m
   . ManageWallet m
  => H.HalogenM State Action () output m (Maybe (NonEmptyArray String))
  -> H.Component query Input output m
component load =
  H.mkComponent
    { initialState: const { texts: Nothing, loading: false }
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
      texts <- load
      H.modify_ \state -> state { loading = false, texts = texts }
      pure unit

  render :: State -> H.ComponentHTML Action () m
  render { texts: Just texts, loading } =
    HH.div [ css "dropdown is-hoverable is-flex" ]
      [ HH.div [ css "dropdown-trigger is-fullwidth" ]
          [ HH.span [ {- aria-haspopup="true" aria-controls="dropdown-menu4" -}]
              [ HH.text $ head texts ]
          , HH.text "… "
          , whenElem loading $ const spinner
          ]
      , HH.div [ css "dropdown-menu is-fullwidth" ]
          [ HH.div [ css "dropdown-content" ]
              [ HH.div [ css "dropdown-item" ]
                  [ HH.div [ css "list" ]
                      [ HH.ul_
                          (renderText <$> toArray texts)
                      ]
                  ]
              ]
          ]
      ]

  render { loading } =
    whenElem loading $ const spinner

  renderText :: ∀ w i. String -> HH.HTML w i
  renderText text =
    HH.div [ css "list-item" ]
      [ HH.li [ css "is-family-monospace" ]
          [ HH.text text ]
      ]
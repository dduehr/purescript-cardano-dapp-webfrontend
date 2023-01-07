module Frontend.Component.HTML.ModalResult (Query(..), component) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (take)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.ARIA as HA

import Frontend.Component.HTML.Utils (css)
import Frontend.Data.Tx (TxId)
import Frontend.Data.Result (Result(..))

import Frontend.Capability.LogMessages (class LogMessages, logMessage)

data Query a = Show (Result TxId) a

type State = Maybe (Result TxId)

data Action
  = Copy TxId
  | Hide

component
  :: ∀ input output m
   . LogMessages m
  => H.Component Query input output m
component =
  H.mkComponent
    { initialState: const Nothing
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleQuery = handleQuery
        , handleAction = handleAction
        }
    }
  where

  render :: State -> H.ComponentHTML Action () m
  render Nothing =
    HH.text ""
  render (Just result) =
    HH.div [ css "modal is-active" ]
      [ HH.div [ css "modal-background" ] []
      , HH.div [ css "modal-content" ]
          [ HH.div [ css "box" ]
              [ HH.article [ css "media" ]
                  [ HH.div [ css "media-left mr-5" ]
                      [ HH.i [ css (icon result), HA.hidden "true" ] [] ]
                  , HH.div [ css "media-content" ]
                      [ HH.div [ css "content" ]
                          [ HH.div [ css "title is-6 mb-2" ]
                              [ HH.text (title result) ]
                          , message result
                          ]
                      ]
                  ]
              ]
          ]
      , HH.button [ css "modal-close is-large", HA.label "close", HE.onClick \_ -> Hide ] []
      ]

  icon :: Result TxId -> String
  icon (Success _) = "fas fa-check has-text-success is-size-1"
  icon Failure = "fas fa-triangle-exclamation has-text-danger is-size-1"

  title :: Result TxId -> String
  title (Success _) = "Success"
  title Failure = "Failure"

  message :: Result TxId -> H.ComponentHTML Action () m
  message (Success txId) =
    HH.p_
      [ HH.text "The transaction was successfully submitted."
      , HH.br_
      , HH.text "Transaction ID: "
      , HH.span [ css "is-family-monospace" ]
          [ HH.text $ take 7 txId ]
      , HH.text "... "
      , HH.a [ HA.label "copy", HE.onClick \_ -> Copy txId ]
          [ HH.span [ css "icon is-small" ]
              [ HH.i [ css "fas fa-copy", HA.hidden "true" ] [] ]
          ]
      ]
  message Failure =
    HH.p_
      [ HH.text "The submission of the transaction failed."
      , HH.br_
      , HH.text "See the console log for details."
      ]

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action () output m (Maybe a)
  handleQuery = case _ of
    Show result a -> do
      H.put $ Just result
      pure $ Just a

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Copy txId ->
      logMessage $ "TODO: copy to clipboard " <> txId
    Hide ->
      H.put Nothing

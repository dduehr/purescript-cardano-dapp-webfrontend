module Frontend.Form.Fields where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isNothing)
import DOM.HTML.Indexed (HTMLi, HTMLinput)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Frontend.Component.HTML.Utils (css, maybeElem)
import Frontend.Data.Wallet (WalletCredentials)
import Frontend.Form.Validation (FormError)

type StringInput action output =
  { state :: F.FieldState String FormError output
  , action :: F.FieldAction action String FormError output
  }

stringInput
  :: ∀ action slots output m
   . String
  -> StringInput action output
  -> Array (HP.IProp HTMLinput action)
  -> Array (HP.IProp HTMLi action)
  -> H.ComponentHTML action slots m
stringInput label { state, action } inputProperties iconProperties =
  HH.div [ css "field" ]
    [ HH.label [ css "label" ]
        [ HH.text label ]
    , HH.div [ css "control has-icons-left" ]
        [ HH.input
            ( append
                [ case state.result of
                    Nothing -> css "input"
                    Just (Left _) -> css "input is-danger"
                    Just (Right _) -> css "input is-success"
                , HP.type_ HP.InputText
                , HP.value state.value
                , HE.onValueInput action.handleChange
                , HE.onBlur action.handleBlur
                ]
                inputProperties
            )
        , HH.span [ css "icon is-small is-left" ]
            [ HH.i iconProperties [] ]
        ]
    , maybeElem (state.result >>= either pure (const Nothing)) \err ->
        HH.div
          [ css "content is-small has-text-danger" ]
          [ HH.text err ]
    ]

submitButton
  :: forall i p
   . String
  -> F.FormState
  -> Maybe WalletCredentials
  -> HH.HTML i p
submitButton label formState mbWalletCredentials =
  HH.div [ css "field" ]
    [ HH.input
        [ css "button is-medium is-success"
        , HP.type_ HP.InputSubmit
        , HP.disabled $ isNothing mbWalletCredentials || not formState.allTouched || formState.errorCount > 0
        , HP.value label
        ]
    ]
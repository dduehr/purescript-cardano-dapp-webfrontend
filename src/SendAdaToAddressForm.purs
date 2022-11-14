module SendAdaToAddressForm (Form, Output, form) where

import Prelude

import Csl as Csl
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( recipient :: f String String Csl.Address
  , amount    :: f String String Csl.BigNum
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

type Output = { | Form F.FieldOutput }

form :: ∀ query m. MonadAff m => H.Component query Unit Output m
form = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Receive context -> H.put context
    Eval action -> F.eval action

  handleQuery :: ∀ a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = do
    let
      validation :: { | Form F.FieldValidation }
      validation =
        { recipient: validBech32
        , amount: validBigNum
        }

    F.handleSubmitValidate F.raise F.validate validation

  validBech32 :: String -> Either String Csl.Address
  validBech32 input =
    case Csl.address.fromBech32 input of
        Just val -> Right val
        _ -> Left "Invalid address"

  validBigNum :: String -> Either String Csl.BigNum
  validBigNum input =
    case Csl.bigNum.fromStr input of
        Just val -> Right val
        _ -> Left "Invalid number"

  render :: FormContext -> H.ComponentHTML Action () m
  render { formActions, fields, actions } =
    HH.form
      [ HE.onSubmit formActions.handleSubmit ]
      [ HH.div_
          [ HH.label_ [ HH.text "Recipient" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueInput actions.recipient.handleChange
              , HE.onBlur actions.recipient.handleBlur
              , case fields.recipient.result of
                  Nothing -> HP.placeholder "Bech32 address"
                  Just (Left _) -> HP.attr (HH.AttrName "aria-invalid") "true"
                  Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
              ]
          , case fields.recipient.result of
              Just (Left err) -> HH.small_ [ HH.text err ]
              _ -> HH.text ""
          ]
      -- FIXME: code duplication
      , HH.div_
          [ HH.label_ [ HH.text "Amount" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueInput actions.amount.handleChange
              , HE.onBlur actions.amount.handleBlur
              , case fields.amount.result of
                  Nothing -> HP.placeholder "Number"
                  Just (Left _) -> HP.attr (HH.AttrName "aria-invalid") "true"
                  Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
              ]
          , case fields.amount.result of
              Just (Left err) -> HH.small_ [ HH.text err ]
              _ -> HH.text ""
          ]
      , HH.button
          [ HP.type_ HP.ButtonSubmit ]
          [ HH.text "Submit" ]
      ]


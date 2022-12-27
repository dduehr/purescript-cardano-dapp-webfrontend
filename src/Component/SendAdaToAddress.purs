module Example.Component.SendAdaToAddress (form) where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.State.Class (get)
import Csl as Csl
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Example.Capability.Resource.Address (class ManageAddress, sendAdaToAddress)
import Example.Component.Utils (css)
import Example.Store as Store

type Input = Store.Store

type State = FormContext

data Action
  = Receive FormContext
  | Eval FormlessAction

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( recipient :: f String String Csl.Address
  , amount    :: f String String Csl.BigNum
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

form
  :: ∀ query output m
   . MonadAff m
  => ManageAddress m
  => H.Component query Input output m
form =
  F.formless { liftAction: Eval } mempty $ H.mkComponent
    { initialState: \context -> context
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
      Receive formContext -> H.put formContext
      Eval formlessAction -> F.eval formlessAction

    handleQuery :: ∀ a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
    handleQuery = do
      let
        validation :: { | Form F.FieldValidation }
        validation =
          { recipient: validBech32
          , amount: validBigNum
          }
      F.handleSubmitValidate onSubmit F.validate validation

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

    onSubmit :: { | Form F.FieldOutput } -> H.HalogenM _ _ _ _ _ Unit
    onSubmit fields = do
      { input } <- get
      -- FIXME: no "case cascade"
      result <- case input.wallet of
        Nothing -> pure $ Left "no wallet"
        Just { api } -> case input.txBuilderConfig of
          Nothing -> pure $ Left "no api"
          Just config -> H.lift $ runExceptT $ sendAdaToAddress api config { recipientAddress: fields.recipient, lovelaceAmount: fields.amount }
      -- TODO: show modal message box    
      case result of
        Left error -> H.lift $ log error
        Right a -> H.lift $ log $ show a
      pure unit

    render :: State -> H.ComponentHTML Action () m
    render { input, formActions, fields, actions } =
      HH.form [ css "pl-3 mt-3" , HE.onSubmit formActions.handleSubmit ]
        [ HH.div [ css "field" ]
          [ HH.label [ css "label" ]
            [ HH.text "Address where to send ADA" ]
          , HH.div [ css "control has-icons-left" ]
            [ HH.input 
              [ case fields.recipient.result of
                  Nothing -> css "input"
                  Just (Left  _) -> css "input is-danger"
                  Just (Right _) -> css "input is-success" 
              , HP.type_ HP.InputText
              , HP.placeholder "e.g. addr_test1qrt..."
              , HP.required true
              , HE.onValueInput actions.recipient.handleChange
              , HE.onBlur actions.recipient.handleBlur
              ]
            , HH.span [ css "icon is-small is-left" ]
              [ HH.i [ css "fa fa-solid fa-address-card" ] [] ]
            ]
          , case fields.recipient.result of
              Just (Left err) -> HH.div [ css "content is-small has-text-danger" ] [ HH.text err ]
              _ -> HH.div_ []
          ]
        , HH.div [ css "field" ]
          [ HH.label [ css "label" ]
            [ HH.text "Lovelaces (1 000 000 Lovelace = 1 ADA)" ]
          , HH.div [ css "control has-icons-left" ]
            [ HH.input 
              [ case fields.amount.result of
                  Nothing -> css "input"
                  Just (Left  _) -> css "input is-danger"
                  Just (Right _) -> css "input is-success" 
              , HP.type_ HP.InputText
              , HP.placeholder "e.g. 1000000"
              , HP.required true 
              , HE.onValueInput actions.amount.handleChange
              , HE.onBlur actions.amount.handleBlur
              ]
            , HH.span [ css "icon is-small is-left" ]
              [ HH.i [ css "fa fa-solid fa-coins" ] [] ]
            ]
          , case fields.amount.result of
              Just (Left err) -> HH.div [ css "content is-small has-text-danger" ] [ HH.text err ]
              _ -> HH.div_ []
          ]
        , HH.div [ css "field" ]
          [ HH.button [ css "button is-medium is-success", HP.disabled $ isNothing input.wallet ] 
            [ HH.text "Submit" ]
          ]
        ]

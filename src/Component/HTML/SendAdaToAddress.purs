module Example.Component.HTML.SendAdaToAddress (component) where

import Prelude

import Control.Monad.State.Class (get)
import Csl as Csl
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectAll)

import Example.Capability.Resource.Address (class ManageAddress, sendAdaToAddress)
import Example.Component.HTML.Utils (css)
import Example.Form.Validation (FormError, bech32Format, bigNumFormat)
import Example.Store as Store

type Input = Unit

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( recipient :: f String FormError Csl.Address
  , amount    :: f String FormError Csl.BigNum
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive (Connected Store.Store FormContext)
  | Eval FormlessAction

type State = 
  { store :: Store.Store
  , form :: FormContext
  }

component
  :: ∀ query output m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageAddress m
  => H.Component query Input output m
component =
  F.formless { liftAction: Eval } mempty $ connect selectAll $ H.mkComponent
    { initialState: deriveState
    , render
    , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
    }
  where

    deriveState :: Connected Store.Store FormContext -> State
    deriveState { context: store, input: form } = { store, form }

    handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
    handleAction = case _ of
      Receive connected -> H.put $ deriveState connected
      Eval formlessAction -> F.eval formlessAction

    handleQuery :: ∀ a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
    handleQuery = do
      let
        validation :: { | Form F.FieldValidation }
        validation =
          { recipient: bech32Format
          , amount: bigNumFormat
          }
      F.handleSubmitValidate onSubmit F.validate validation

    onSubmit :: { | Form F.FieldOutput } -> H.HalogenM _ _ _ _ _ Unit
    onSubmit fields = do
      { store } <- get
      for_ store.wallet \wallet -> do
        mbTxId <- sendAdaToAddress wallet.api { recipientAddress: fields.recipient, lovelaceAmount: fields.amount }
        log $ "sendAdaToAddress: " <> fromMaybe "failed" mbTxId 
      pure unit  

    render :: State -> H.ComponentHTML Action () m
    render { store: { wallet }, form: { formActions, fields, actions } } =
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
          [ HH.button [ css "button is-medium is-success", HP.disabled $ isNothing wallet ] 
            [ HH.text "Submit" ]
          ]
        ]

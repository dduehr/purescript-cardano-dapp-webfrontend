module Frontend.Component.HTML.SendAdaToAddress (component) where

import Prelude

import Csl as CS
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectAll)

import Frontend.Capability.Domain.Address (class ManageAddress, sendAdaToAddress)
import Frontend.Component.HTML.Utils (css)
import Frontend.Data.Tx (TxId)
import Frontend.Form.Validation (FormError, bech32Format, bigNumFormat, notLessThanBigNum, requiredText)
import Frontend.Store as Store

type Input = Unit

type Output = Maybe TxId

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( recipientAddress :: f String FormError CS.Address
  , lovelaceAmount :: f String FormError CS.BigNum
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
  :: ∀ query m
   . MonadAff m
  => ManageAddress m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input Output m
component =
  F.formless { liftAction: Eval, validateOnChange: true } mempty $ connect selectAll $ H.mkComponent
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
        { recipientAddress: bech32Format <=< requiredText
        , lovelaceAmount: (notLessThanBigNum "the minimum UTXO value" "969750") <=< bigNumFormat <=< requiredText
        }
    F.handleSubmitValidate onSubmit F.validate validation

  onSubmit :: { | Form F.FieldOutput } -> H.HalogenM _ _ _ _ _ Unit
  onSubmit fields = do
    { store: { mbWalletCredentials } } <- H.get
    for_ mbWalletCredentials \walletCredentials -> do
      mbTxId <- sendAdaToAddress walletCredentials.api
        { recipientAddress: fields.recipientAddress
        , lovelaceAmount: fields.lovelaceAmount
        }
      F.raise mbTxId

  render :: State -> H.ComponentHTML Action () m
  render { store: { mbWalletCredentials }, form: { formState, formActions, fields, actions } } =
    HH.form [ css "pl-3 mt-3", HE.onSubmit formActions.handleSubmit ]
      [ HH.div [ css "field" ]
          [ HH.label [ css "label" ]
              [ HH.text "Address where to send ADA" ]
          , HH.div [ css "control has-icons-left" ]
              [ HH.input
                  [ case fields.recipientAddress.result of
                      Nothing -> css "input"
                      Just (Left _) -> css "input is-danger"
                      Just (Right _) -> css "input is-success"
                  , HP.type_ HP.InputText
                  , HP.placeholder "e.g. addr_test1qrt..."
                  , HE.onValueInput actions.recipientAddress.handleChange
                  , HE.onBlur actions.recipientAddress.handleBlur
                  ]
              , HH.span [ css "icon is-small is-left" ]
                  [ HH.i [ css "fas fa-solid fa-address-card" ] [] ]
              ]
          , case fields.recipientAddress.result of
              Just (Left err) -> HH.div [ css "content is-small has-text-danger" ] [ HH.text err ]
              _ -> HH.div_ []
          ]
      , HH.div [ css "field" ]
          [ HH.label [ css "label" ]
              [ HH.text "Lovelaces (1 000 000 Lovelace = 1 ADA)" ]
          , HH.div [ css "control has-icons-left" ]
              [ HH.input
                  [ case fields.lovelaceAmount.result of
                      Nothing -> css "input"
                      Just (Left _) -> css "input is-danger"
                      Just (Right _) -> css "input is-success"
                  , HP.type_ HP.InputText
                  , HP.placeholder "e.g. 1000000"
                  , HE.onValueInput actions.lovelaceAmount.handleChange
                  , HE.onBlur actions.lovelaceAmount.handleBlur
                  ]
              , HH.span [ css "icon is-small is-left" ]
                  [ HH.i [ css "fas fa-solid fa-coins" ] [] ]
              ]
          , case fields.lovelaceAmount.result of
              Just (Left err) -> HH.div [ css "content is-small has-text-danger" ] [ HH.text err ]
              _ -> HH.div_ []
          ]
      , HH.div [ css "field" ]
          [ HH.button
              [ css "button is-medium is-success"
              , HP.disabled $ isNothing mbWalletCredentials || not formState.allTouched || formState.errorCount > 0
              ]
              [ HH.text "Submit" ]
          ]
      ]

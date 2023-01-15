module Frontend.Component.HTML.SendAdaToContract (component) where

import Prelude

import Csl as CS
import Data.Foldable (for_)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectAll)

import Frontend.Api.Domain.Contract (class ManageContract, sendAdaToContract)
import Frontend.Component.HTML.Utils (css)
import Frontend.Data.Tx (TxId)
import Frontend.Form.Fields (stringInput, submitButton) as Fields
import Frontend.Form.Validation (FormError, bech32Format, bigNumFormat, notLessThanBigNum, requiredText)
import Frontend.Store as Store

type Input = Unit

type Output = Maybe TxId

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( contractAddress :: f String FormError CS.Address
  , lovelaceAmount :: f String FormError CS.BigNum
  , mbDatum :: f String FormError (Maybe String)
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
  => ManageContract m
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
        { contractAddress: bech32Format <=< requiredText
        , lovelaceAmount: (notLessThanBigNum "the minimum UTXO value" "969750") <=< bigNumFormat <=< requiredText
        , mbDatum: Right <<< Just
        }
    F.handleSubmitValidate onSubmit F.validate validation

  onSubmit :: { | Form F.FieldOutput } -> H.HalogenM _ _ _ _ _ Unit
  onSubmit fields = do
    { store: { mbWalletCredentials } } <- H.get
    for_ mbWalletCredentials \walletCredentials -> do
      mbTxId <- sendAdaToContract walletCredentials.api
        { contractAddress: fields.contractAddress
        , lovelaceAmount: fields.lovelaceAmount
        , mbDatum: fields.mbDatum
        }
      F.raise mbTxId

  render :: State -> H.ComponentHTML Action () m
  render { store: { mbWalletCredentials }, form: { formState, formActions, fields, actions } } =
    HH.form [ css "pl-3 mt-3", HE.onSubmit formActions.handleSubmit ]
      [ Fields.stringInput "Contract where to send ADA"
          { state: fields.contractAddress, action: actions.contractAddress }
          [ HP.placeholder "e.g. addr_test1qrt…", HP.required true ]
          [ css "fas fa-solid fa-address-card" ]
      , Fields.stringInput "Lovelaces (1 000 000 Lovelace = 1 ADA)"
          { state: fields.lovelaceAmount, action: actions.lovelaceAmount }
          [ HP.placeholder "e.g. 1500000", HP.required true ]
          [ css "fas fa-solid fa-coins" ]
      , Fields.stringInput "Optional datum required by the redeemer to unlock the ADA from the contract again"
          { state: fields.mbDatum, action: actions.mbDatum }
          [ HP.placeholder "Top Secret" ]
          [ css "fas fa-solid fa-key" ]
      , Fields.submitButton "Submit" \_ -> formState.errorCount == 0
          && isJust fields.contractAddress.result
          && isJust fields.lovelaceAmount.result
          && isJust mbWalletCredentials
      ]

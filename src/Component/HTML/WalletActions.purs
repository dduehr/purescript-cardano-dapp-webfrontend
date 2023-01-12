module Frontend.Component.HTML.WalletActions where

import Prelude

import Data.Bounded.Generic (genericBottom)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.Show.Generic (genericShow)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Monad (class MonadStore)
import Type.Proxy (Proxy(..))

import Frontend.Api.Utils (enumValues)
import Frontend.Capability.Domain.Address (class ManageAddress)
import Frontend.Capability.Domain.Browser (class ManageBrowser)
import Frontend.Capability.Domain.Contract (class ManageContract)
import Frontend.Capability.Domain.Wallet (class ManageWallet)
import Frontend.Capability.LogMessages (class LogMessages)
import Frontend.Component.HTML.ModalResult (Query(..), component) as ModalResult
import Frontend.Component.HTML.RedeemAdaFromContract (component) as RedeemAdaFromContract
import Frontend.Component.HTML.RedeemTokenFromContract (component) as RedeemTokenFromContract
import Frontend.Component.HTML.SendAdaToAddress (component) as SendAdaToAddress
import Frontend.Component.HTML.SendAdaToContract (component) as SendAdaToContract
import Frontend.Component.HTML.SendTokenToAddress (component) as SendTokenToAddress
import Frontend.Component.HTML.SendTokenToContract (component) as SendTokenToContract
import Frontend.Component.HTML.Utils (css, whenElem)
import Frontend.Data.Result (Result(..))
import Frontend.Data.Tx (TxId)
import Frontend.Store as Store

type State =
  { isDropDownActive :: Boolean
  , menuItemSelected :: MenuItem
  }

data MenuItem
  = SendAdaToAddress
  | SendTokenToAddress
  | SendAdaToContract
  | SendTokenToContract
  | RedeemAdaFromContract
  | RedeemTokenFromContract

derive instance Eq MenuItem
derive instance Generic MenuItem _
instance Show MenuItem where
  show = genericShow

data Action
  = ToggleMenu
  | Select MenuItem
  | HandleResult (Maybe TxId)

type Slots =
  ( sendAdaToAddress :: ∀ query. H.Slot query (Maybe TxId) Unit
  , sendTokenToAddress :: ∀ query. H.Slot query (Maybe TxId) Unit
  , sendAdaToContract :: ∀ query. H.Slot query (Maybe TxId) Unit
  , sendTokenToContract :: ∀ query. H.Slot query (Maybe TxId) Unit
  , redeemAdaFromContract :: ∀ query. H.Slot query (Maybe TxId) Unit
  , redeemTokenFromContract :: ∀ query. H.Slot query (Maybe TxId) Unit
  , modalResult :: H.Slot ModalResult.Query Void Unit
  )

component
  :: ∀ query input output m
   . MonadAff m
  => ManageBrowser m
  => ManageWallet m
  => ManageAddress m
  => ManageContract m
  => LogMessages m
  => MonadStore Store.Action Store.Store m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState: const $ { isDropDownActive: false, menuItemSelected: genericBottom }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where

  render :: State -> H.ComponentHTML Action Slots m
  render { isDropDownActive, menuItemSelected } =
    HH.div [ css "card p-4" ]
      [ HH.div [ css ("dropdown" <> if isDropDownActive then " is-active" else "") ]
          [ HH.div [ css "dropdown-trigger" ]
              -- FIXME: aria...
              [ HH.h3 [ css "title is-5 pl-0 ml-0 mb-3 is-clickable" {-, aria-haspopup="true" aria-controls="dropdown-menu" -} , HE.onClick \_ -> ToggleMenu ]
                  [ HH.text $ menuLabel menuItemSelected
                  , HH.text " "
                  , HH.span [ css "icon is-small" ]
                      [ HH.i [ css ("fas " <> if isDropDownActive then "fa-angle-up" else "fa-angle-down") {-, aria-hidden="true" -} ] [] ]
                  ]
              ]
          -- FIXME: aria...
          , whenElem isDropDownActive \_ -> HH.div [ css "dropdown-menu" {-, id="dropdown-menu" role="menu" -} ]
              [ HH.div [ css "dropdown-content" ]
                  (renderMenuLabelWithDivider menuItemSelected <$> enumValues)
              ]
          ]
      , HH.div_
          (renderMenuContent menuItemSelected <$> enumValues)
      , HH.slot_ (Proxy :: _ "modalResult") unit ModalResult.component unit
      ]

  renderMenuLabel :: MenuItem -> MenuItem -> H.ComponentHTML Action Slots m
  renderMenuLabel selected menuItem =
    if menuItem == selected then HH.a [ css "dropdown-item is-active" ]
      [ HH.text $ menuLabel menuItem ]
    else HH.a [ css "dropdown-item", HE.onClick \_ -> Select menuItem ]
      [ HH.text $ menuLabel menuItem ]

  renderMenuLabelWithDivider :: MenuItem -> MenuItem -> H.ComponentHTML Action Slots m
  renderMenuLabelWithDivider selected menuItem =
    if menuItem == SendTokenToAddress then HH.div_
      [ renderMenuLabel selected menuItem
      , HH.hr [ css "dropdown-divider" ]
      ]
    else renderMenuLabel selected menuItem

  renderMenuContent :: MenuItem -> MenuItem -> H.ComponentHTML Action Slots m
  renderMenuContent selected menuItem =
    if menuItem == selected then HH.div_
      [ menuContent menuItem ]
    else HH.div [ css "is-hidden" ]
      [ menuContent menuItem ]

  menuLabel :: MenuItem -> String
  menuLabel SendAdaToAddress = "Send ADA to Public Key Address"
  menuLabel SendTokenToAddress = "Send Token to Public Key Address"
  menuLabel SendAdaToContract = "Send ADA to Smart Contract"
  menuLabel SendTokenToContract = "Send Token to Smart Contract"
  menuLabel RedeemAdaFromContract = "Redeem ADA from Smart Contract"
  menuLabel RedeemTokenFromContract = "Redeem Token from Smart Contract"

  menuContent :: MenuItem -> H.ComponentHTML Action Slots m
  menuContent SendAdaToAddress = HH.slot (Proxy :: _ "sendAdaToAddress") unit SendAdaToAddress.component unit HandleResult
  menuContent SendTokenToAddress = HH.slot_ (Proxy :: _ "sendTokenToAddress") unit SendTokenToAddress.component unit
  menuContent SendAdaToContract = HH.slot_ (Proxy :: _ "sendAdaToContract") unit SendAdaToContract.component unit
  menuContent SendTokenToContract = HH.slot_ (Proxy :: _ "sendTokenToContract") unit SendTokenToContract.component unit
  menuContent RedeemAdaFromContract = HH.slot_ (Proxy :: _ "redeemAdaFromContract") unit RedeemAdaFromContract.component unit
  menuContent RedeemTokenFromContract = HH.slot_ (Proxy :: _ "redeemTokenFromContract") unit RedeemTokenFromContract.component unit

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    ToggleMenu ->
      H.modify_ \state -> state { isDropDownActive = not state.isDropDownActive }
    Select menuItem ->
      H.put $ { isDropDownActive: false, menuItemSelected: menuItem }
    HandleResult result ->
      H.tell (Proxy :: _ "modalResult") unit (ModalResult.Show $ maybe Failure Success result)

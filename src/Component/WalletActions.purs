module Example.Component.WalletActions where

import Prelude

import Data.Bounded.Generic (genericBottom)
import Data.Enum.Generic (genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Monad (class MonadStore)
import Type.Proxy (Proxy(..))

import Example.Capability.Resource.Address (class ManageAddress)
import Example.Capability.Resource.Contract (class ManageContract)
import Example.Capability.Resource.WebPage (class ManageWebPage)
import Example.Component.RedeemAdaFromContract (component) as RedeemAdaFromContract 
import Example.Component.RedeemTokenFromContract (component) as RedeemTokenFromContract 
import Example.Component.SendAdaToAddress (form) as SendAdaToAddress 
import Example.Component.SendAdaToContract (component) as SendAdaToContract 
import Example.Component.SendTokenToAddress (component) as SendTokenToAddress 
import Example.Component.SendTokenToContract (component) as SendTokenToContract
import Example.Component.Utils (css)
import Example.Store (Action, Store) as Store

data State 
  = Selected MenuItem

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
  = Select MenuItem

type Slots =
  ( sendAdaToAddress :: ∀ query. H.Slot query Void Unit
  , sendTokenToAddress :: ∀ query. H.Slot query Void Unit
  , sendAdaToContract :: ∀ query. H.Slot query Void Unit
  , sendTokenToContract :: ∀ query. H.Slot query Void Unit
  , redeemAdaFromContract :: ∀ query. H.Slot query Void Unit
  , redeemTokenFromContract :: ∀ query. H.Slot query Void Unit
  )

component
  :: ∀ query input output m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageWebPage m
  => ManageAddress m
  => ManageContract m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState: const $ Selected genericBottom 
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where

    render :: State -> H.ComponentHTML Action Slots m
    render (Selected selected) =
      HH.div [ css "card p-4" ]
        [ HH.div [ css "dropdown is-hoverable" ]
            [ HH.div [ css "dropdown-trigger" ]
                [ HH.h3 [ css "title is-5 pl-0 ml-0 mb-3" {-, aria-haspopup="true" aria-controls="dropdown-menu" -} ]
                    [ HH.text $ menuLabel selected
                    , HH.text " "
                    , HH.span [ css "icon is-small" ]
                        [ HH.i [ css "fas fa-angle-down" {-, aria-hidden="true" -} ] [] ]
                    ]
                ]
            , HH.div [ css "dropdown-menu" {-, id="dropdown-menu" role="menu" -} ]
                [ HH.div [ css "dropdown-content" ]
                    ( renderMenuLabelWithDivider selected <$> menuItems )
                ]
            ]
        , HH.div_
            ( renderMenuContent selected <$> menuItems )
        ]

    renderMenuLabel :: MenuItem -> MenuItem -> H.ComponentHTML Action Slots m
    renderMenuLabel selected menuItem =
      if menuItem == selected
        then HH.a [ css "dropdown-item is-active" ]
               [ HH.text $ menuLabel menuItem ]
        else HH.a [ css "dropdown-item", HE.onClick \_ -> Select menuItem ]
               [ HH.text $ menuLabel menuItem ]

    renderMenuLabelWithDivider :: MenuItem -> MenuItem -> H.ComponentHTML Action Slots m
    renderMenuLabelWithDivider selected menuItem =
      if menuItem == SendTokenToAddress
        then HH.div_
               [ renderMenuLabel selected menuItem 
               , HH.hr [ css "dropdown-divider" ]
               ]
        else renderMenuLabel selected menuItem

    renderMenuContent :: MenuItem -> MenuItem -> H.ComponentHTML Action Slots m
    renderMenuContent selected menuItem =
      if menuItem == selected
        then HH.div_ 
               [ menuContent menuItem ]
        else HH.div [ css "is-hidden" ]
               [ menuContent menuItem ]

    menuItems :: Array MenuItem
    menuItems = unfoldr (\maybeItem -> maybeItem >>= next) $ Just genericBottom
      where
        next menuItem = Just $ Tuple menuItem $ genericSucc menuItem

    menuLabel :: MenuItem -> String
    menuLabel SendAdaToAddress        = "Send ADA to Public Key Address"
    menuLabel SendTokenToAddress      = "Send Token to Public Key Address"
    menuLabel SendAdaToContract       = "Send ADA to Smart Contract"
    menuLabel SendTokenToContract     = "Send Token to Smart Contract"
    menuLabel RedeemAdaFromContract   = "Redeem ADA from Smart Contract"
    menuLabel RedeemTokenFromContract = "Redeem Token from Smart Contract"

    menuContent :: MenuItem -> H.ComponentHTML Action Slots m
    menuContent SendAdaToAddress        = HH.slot_ (Proxy :: _ "sendAdaToAddress") unit SendAdaToAddress.form unit
    menuContent SendTokenToAddress      = HH.slot_ (Proxy :: _ "sendTokenToAddress") unit SendTokenToAddress.component unit 
    menuContent SendAdaToContract       = HH.slot_ (Proxy :: _ "sendAdaToContract") unit SendAdaToContract.component unit
    menuContent SendTokenToContract     = HH.slot_ (Proxy :: _ "sendTokenToContract") unit SendTokenToContract.component unit
    menuContent RedeemAdaFromContract   = HH.slot_ (Proxy :: _ "redeemAdaFromContract") unit RedeemAdaFromContract.component unit
    menuContent RedeemTokenFromContract = HH.slot_ (Proxy :: _ "redeemTokenFromContract") unit RedeemTokenFromContract.component unit

    handleAction :: Action -> H.HalogenM State Action Slots output m Unit
    handleAction = case _ of
      Select menuItem ->
        H.put $ Selected menuItem
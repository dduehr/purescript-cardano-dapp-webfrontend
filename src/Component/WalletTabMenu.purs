module Example.Component.WalletTabMenu where

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
import Example.Component.SendAdaToAddress (component) as SendAdaToAddress 
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
      HH.div [ css "columns" ]
        [ HH.div [ css "column is-4-tablet is-3-desktop is-2-widescreen" ]
          [ HH.nav [ css "menu" ]
            [ HH.p [ css "menu-label" ]
              [ HH.text "Actions" ]
            ]
          , HH.ul [ css "menu-list" ] 
              ( renderMenuLabel selected <$> menuItems )
          ]
        , HH.div [ css "column" ]
            ( renderMenuTab selected <$> menuItems )
        ]

    renderMenuLabel :: MenuItem -> MenuItem -> H.ComponentHTML Action Slots m
    renderMenuLabel selected menuItem =
      HH.li_
        [ HH.a [ css if menuItem == selected then "is-active" else "", HE.onClick \_ -> Select menuItem ]
          [ HH.text $ menuLabel menuItem ]
        ]

    renderMenuTab :: MenuItem -> MenuItem -> H.ComponentHTML Action Slots m
    renderMenuTab selected menuItem =
      HH.div [ css if menuItem /= selected then "is-hidden" else "" ]
        [ menuTab menuItem ]

    menuItems :: Array MenuItem
    menuItems = unfoldr (\maybeItem -> maybeItem >>= next) $ Just genericBottom
      where
        next menuItem = Just $ Tuple menuItem $ genericSucc menuItem

    menuLabel :: MenuItem -> String
    menuLabel SendAdaToAddress        = "Send ADA to Address"
    menuLabel SendTokenToAddress      = "Send Token to Address"
    menuLabel SendAdaToContract       = "Send ADA to Contract"
    menuLabel SendTokenToContract     = "Send Token to Contract"
    menuLabel RedeemAdaFromContract   = "Redeem ADA from Contract"
    menuLabel RedeemTokenFromContract = "Redeem Token from Contract"

    menuTab :: MenuItem -> H.ComponentHTML Action Slots m
    menuTab SendAdaToAddress        = HH.slot_ (Proxy :: _ "sendAdaToAddress") unit SendAdaToAddress.component unit
    menuTab SendTokenToAddress      = HH.slot_ (Proxy :: _ "sendTokenToAddress") unit SendTokenToAddress.component unit 
    menuTab SendAdaToContract       = HH.slot_ (Proxy :: _ "sendAdaToContract") unit SendAdaToContract.component unit
    menuTab SendTokenToContract     = HH.slot_ (Proxy :: _ "sendTokenToContract") unit SendTokenToContract.component unit
    menuTab RedeemAdaFromContract   = HH.slot_ (Proxy :: _ "redeemAdaFromContract") unit RedeemAdaFromContract.component unit
    menuTab RedeemTokenFromContract = HH.slot_ (Proxy :: _ "redeemTokenFromContract") unit RedeemTokenFromContract.component unit

    handleAction :: Action -> H.HalogenM State Action Slots output m Unit
    handleAction = case _ of
      Select menuItem ->
        H.put $ Selected menuItem
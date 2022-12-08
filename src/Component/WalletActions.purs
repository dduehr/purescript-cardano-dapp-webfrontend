module Qapla.Component.WalletActions where

import Prelude

import Cardano.Wallet (Api, WalletName)
import Cardano.Wallet (enable) as CW
import Data.Bounded.Generic (genericBottom)
import Data.Enum.Generic (genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Qapla.Api.WalletName (unwrap) as WalletName
import Qapla.Component.RedeemAdaFromContract (component) as RedeemAdaFromContract 
import Qapla.Component.RedeemTokenFromContract (component) as RedeemTokenFromContract 
import Qapla.Component.SendAdaToAddress (component) as SendAdaToAddress 
import Qapla.Component.SendAdaToContract (component) as SendAdaToContract 
import Qapla.Component.SendTokenToAddress (component) as SendTokenToAddress 
import Qapla.Component.SendTokenToContract (component) as SendTokenToContract
import Qapla.Component.Utils (css)
import Type.Proxy (Proxy(..))

data Action 
  = Receive Input
  | Select MenuItem

type Input = Maybe WalletName

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

data State 
  = Received Input
  | Enabled Api MenuItem
  | Failed Message

type Message = String

type Slots =
  ( sendAdaToAddress :: ∀ query. H.Slot query Void Unit
  , sendTokenToAddress :: ∀ query. H.Slot query Void Unit
  , sendAdaToContract :: ∀ query. H.Slot query Void Unit
  , sendTokenToContract :: ∀ query. H.Slot query Void Unit
  , redeemAdaFromContract :: ∀ query. H.Slot query Void Unit
  , redeemTokenFromContract :: ∀ query. H.Slot query Void Unit
  )

component :: ∀ query output m. MonadAff m => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }

initialState :: Input -> State
initialState = Received

render :: ∀ m. MonadAff m => State -> H.ComponentHTML Action Slots m
render (Received input) = renderInput input
render (Enabled _ menuItem) = renderMenu menuItem
render (Failed message) = HH.text message

renderInput :: ∀ w i. Input -> HH.HTML w i
renderInput Nothing = 
  HH.text ""
renderInput (Just walletName) =
  HH.span_
    [ HH.text "Wallet "
    , HH.text $ WalletName.unwrap walletName
    , HH.text " selected ..."
    ]

renderMenu :: ∀ m. MonadAff m => MenuItem -> H.ComponentHTML Action Slots m
renderMenu enabled =
  HH.div [ css "columns" ]
    [ HH.div [ css "column is-4-tablet is-3-desktop is-2-widescreen" ]
      [ HH.nav [ css "menu" ]
        [ HH.p [ css "menu-label" ]
          [ HH.text "Actions" ]
        ]
      , HH.ul [ css "menu-list" ] 
          ( renderMenuItem enabled <$> menuItems )
      ]
    , HH.div [ css "column" ]
        ( renderMenuComponent enabled Nothing <$> menuItems )
    ]

renderMenuItem :: ∀ m. MonadAff m => MenuItem -> MenuItem -> H.ComponentHTML Action Slots m
renderMenuItem enabled menuItem =
  HH.li_
    [ HH.a [ css if menuItem == enabled then "is-active" else "", HE.onClick \_ -> Select menuItem ]
      [ HH.text $ menuLabel menuItem ]
    ]

renderMenuComponent :: ∀ m. MonadAff m => MenuItem -> Maybe Api -> MenuItem -> H.ComponentHTML Action Slots m
renderMenuComponent enabled input menuItem =
  HH.div [ css if menuItem /= enabled then "is-hidden" else "" ]
    [ menuComponent input menuItem ]

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

menuComponent :: ∀ m. MonadAff m => Maybe Api -> MenuItem -> H.ComponentHTML Action Slots m
menuComponent input SendAdaToAddress        = HH.slot_ (Proxy :: _ "sendAdaToAddress") unit SendAdaToAddress.component input
menuComponent input SendTokenToAddress      = HH.slot_ (Proxy :: _ "sendTokenToAddress") unit SendTokenToAddress.component input 
menuComponent input SendAdaToContract       = HH.slot_ (Proxy :: _ "sendAdaToContract") unit SendAdaToContract.component input
menuComponent input SendTokenToContract     = HH.slot_ (Proxy :: _ "sendTokenToContract") unit SendTokenToContract.component input
menuComponent input RedeemAdaFromContract   = HH.slot_ (Proxy :: _ "redeemAdaFromContract") unit RedeemAdaFromContract.component input
menuComponent input RedeemTokenFromContract = HH.slot_ (Proxy :: _ "redeemTokenFromContract") unit RedeemTokenFromContract.component input

handleAction :: ∀ output m. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  Receive Nothing ->
    H.put $ Received Nothing
  Receive (Just walletName) -> do
    api <- liftAff $ CW.enable walletName
    H.put $ Enabled api SendAdaToAddress
  Select menuItem -> do
    log $ show menuItem <> " selected"
    state <- H.get
    case state of
      Enabled api _ -> H.put $ Enabled api menuItem
      _ -> pure unit
module Qapla.Page.Root (component) where

import Prelude

import Cardano.Wallet (WalletName)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Qapla.Component.Utils (css)
import Qapla.Component.WalletActions (component) as WalletActions
import Qapla.Component.WalletView (component) as WalletView
import Qapla.Component.WalletsDropDown (component) as WalletsDropDown
import Type.Proxy (Proxy(..))

data Action
  = HandleWalletsDropDown (Maybe WalletName)

type State = Maybe WalletName

type Slots =
  ( walletsDropDown :: ∀ query. H.Slot query (Maybe WalletName) Unit
  , walletView :: ∀ query. H.Slot query Void Unit
  , walletActions :: ∀ query. H.Slot query Void Unit
  )

component :: ∀ query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: const Nothing
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: ∀ m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div_
    [ HH.nav [ css "navbar has-shadow" ]
        [ HH.div [ css "navbar-brand" ]
            [ HH.div [ css "navbar-item" ] 
              [ HH.img [ HP.src "images/cardano-ada-logo.png" ] ]
            ]
        , HH.div [ css "navbar-menu" ]
            [ HH.div [ css "navbar-start" ]
                [ HH.div [ css "navbar-item" ]
                  [ HH.small_ 
                      [ HH.text "Cardano DApp Connection Example" ]
                  ]
                ]
            , HH.div [ css "navbar-end" ]
                [ HH.slot (Proxy :: _ "walletsDropDown") unit WalletsDropDown.component unit HandleWalletsDropDown ]
            ]
        ]
    , HH.section [ css "section" ] 
        [ HH.slot_ (Proxy :: _ "walletView") unit WalletView.component state ]
    , HH.section [ css "section" ] 
        [ HH.slot_ (Proxy :: _ "walletActions") unit WalletActions.component state ]
    ]

handleAction :: ∀ output m. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  HandleWalletsDropDown walletName -> do
    H.put walletName
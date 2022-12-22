module Example.Page.Root (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Type.Proxy (Proxy(..))

import Example.Capability.Resource.Address (class ManageAddress)
import Example.Capability.Resource.Contract (class ManageContract)
import Example.Capability.Resource.Wallet (class ManageWallet)
import Example.Capability.Resource.WebPage (class ManageWebPage)
import Example.Component.Utils (css)
import Example.Component.WalletTabMenu (component) as WalletTabMenu
import Example.Component.WalletView (component) as WalletView
import Example.Component.WalletsDropDown (component) as WalletsDropDown
import Example.Store (Action, Store) as Store

type Slots =
  ( walletsDropDown :: ∀ query. H.Slot query Void Unit
  , walletView :: ∀ query. H.Slot query Void Unit
  , walletTabMenu :: ∀ query. H.Slot query Void Unit
  )

component 
  :: ∀ query input output m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageWebPage m
  => ManageWallet m
  => ManageAddress m
  => ManageContract m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState: const Nothing
    , render
    , eval: H.mkEval H.defaultEval 
    }
  where

    render :: ∀ action state. state -> H.ComponentHTML action Slots m
    render _ =
      HH.div_
        [ HH.nav [ css "navbar has-shadow" ]
            [ HH.div [ css "navbar-brand" ]
                [ HH.div [ css "navbar-item" ] 
                  [ HH.img [ HP.src "data:image/png;base64,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" ] ]
                ]
            , HH.div [ css "navbar-menu" ]
                [ HH.div [ css "navbar-start" ]
                    [ HH.div [ css "navbar-item has-text-weight-bold" ]
                      [ HH.small_ 
                          [ HH.text "Cardano DApp Connection Example" ]
                      ]
                    ]
                , HH.div [ css "navbar-end" ]
                    [ HH.slot_ (Proxy :: _ "walletsDropDown") unit WalletsDropDown.component unit ]
                ]
            ]
        , HH.section [ css "section" ] 
            [ HH.slot_ (Proxy :: _ "walletView") unit WalletView.component unit ]
        , HH.section [ css "section" ] 
            [ HH.slot_ (Proxy :: _ "walletTabMenu") unit WalletTabMenu.component unit ]
        ]
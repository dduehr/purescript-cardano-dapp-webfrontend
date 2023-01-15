module Frontend.Page.Root (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Type.Proxy (Proxy(..))

import Frontend.Api.Domain.Address (class ManageAddress)
import Frontend.Api.Domain.Browser (class ManageBrowser)
import Frontend.Api.Domain.Contract (class ManageContract)
import Frontend.Api.Domain.Wallet (class ManageWallet)
import Frontend.Api.LogMessages (class LogMessages)
import Frontend.Component.HTML.Utils (css)
import Frontend.Component.HTML.WalletActions (component) as WalletActions
import Frontend.Component.HTML.WalletView (component, Query(..)) as WalletView
import Frontend.Component.HTML.WalletsDropDown (component, Message(..)) as WalletsDropDown
import Frontend.Store (Action, Store) as Store

data Action = HandleWalletsDropDown WalletsDropDown.Message

type Slots =
  ( walletsDropDown :: ∀ query. H.Slot query WalletsDropDown.Message Unit
  , walletActions :: ∀ query. H.Slot query Void Unit
  , walletView :: H.Slot WalletView.Query Void Unit
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
    { initialState: const Nothing
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction }
    }
  where

  handleAction :: ∀ state. Action -> H.HalogenM state Action Slots output m Unit
  handleAction = case _ of
    HandleWalletsDropDown WalletsDropDown.ReloadWallet ->
      H.tell (Proxy :: _ "walletView") unit WalletView.ReloadWallet

  render :: ∀ state. state -> H.ComponentHTML Action Slots m
  render _ =
    HH.div_
      [ HH.nav [ css "navbar has-shadow" ]
          [ HH.div [ css "navbar-brand" ]
              [ HH.div [ css "navbar-item" ]
                  [ HH.img [ HP.src "data:image/png;base64,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" ] ]
              ]
          , HH.div [ css "navbar-menu" ]
              [ HH.div [ css "navbar-start" ]
                  [ HH.div [ css "navbar-item title is-5" ]
                      [ HH.text "Cardano DApp Web-Frontend"
                      , HH.a [ css "icon is-small ml-2", HP.href "https://github.com/dduehr/purescript-cardano-dapp-webfrontend" ]
                          [ HH.img [ HP.src "data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iOTgiIGhlaWdodD0iOTYiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+PHBhdGggZmlsbC1ydWxlPSJldmVub2RkIiBjbGlwLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik00OC44NTQgMEMyMS44MzkgMCAwIDIyIDAgNDkuMjE3YzAgMjEuNzU2IDEzLjk5MyA0MC4xNzIgMzMuNDA1IDQ2LjY5IDIuNDI3LjQ5IDMuMzE2LTEuMDU5IDMuMzE2LTIuMzYyIDAtMS4xNDEtLjA4LTUuMDUyLS4wOC05LjEyNy0xMy41OSAyLjkzNC0xNi40Mi01Ljg2Ny0xNi40Mi01Ljg2Ny0yLjE4NC01LjcwNC01LjQyLTcuMTctNS40Mi03LjE3LTQuNDQ4LTMuMDE1LjMyNC0zLjAxNS4zMjQtMy4wMTUgNC45MzQuMzI2IDcuNTIzIDUuMDUyIDcuNTIzIDUuMDUyIDQuMzY3IDcuNDk2IDExLjQwNCA1LjM3OCAxNC4yMzUgNC4wNzQuNDA0LTMuMTc4IDEuNjk5LTUuMzc4IDMuMDc0LTYuNi0xMC44MzktMS4xNDEtMjIuMjQzLTUuMzc4LTIyLjI0My0yNC4yODMgMC01LjM3OCAxLjk0LTkuNzc4IDUuMDE0LTEzLjItLjQ4NS0xLjIyMi0yLjE4NC02LjI3NS40ODYtMTMuMDM4IDAgMCA0LjEyNS0xLjMwNCAxMy40MjYgNS4wNTJhNDYuOTcgNDYuOTcgMCAwIDEgMTIuMjE0LTEuNjNjNC4xMjUgMCA4LjMzLjU3MSAxMi4yMTMgMS42MyA5LjMwMi02LjM1NiAxMy40MjctNS4wNTIgMTMuNDI3LTUuMDUyIDIuNjcgNi43NjMuOTcgMTEuODE2LjQ4NSAxMy4wMzggMy4xNTUgMy40MjIgNS4wMTUgNy44MjIgNS4wMTUgMTMuMiAwIDE4LjkwNS0xMS40MDQgMjMuMDYtMjIuMzI0IDI0LjI4MyAxLjc4IDEuNTQ4IDMuMzE2IDQuNDgxIDMuMzE2IDkuMTI2IDAgNi42LS4wOCAxMS44OTctLjA4IDEzLjUyNiAwIDEuMzA0Ljg5IDIuODUzIDMuMzE2IDIuMzY0IDE5LjQxMi02LjUyIDMzLjQwNS0yNC45MzUgMzMuNDA1LTQ2LjY5MUM5Ny43MDcgMjIgNzUuNzg4IDAgNDguODU0IDB6IiBmaWxsPSIjMjQyOTJmIi8+PC9zdmc+" ] ]
                      ]
                  ]
              , HH.div [ css "navbar-end" ]
                  [ HH.slot (Proxy :: _ "walletsDropDown") unit WalletsDropDown.component unit HandleWalletsDropDown ]
              ]
          ]
      , HH.section [ css "section pt-6 pb-0" ]
          [ HH.slot_ (Proxy :: _ "walletActions") unit WalletActions.component unit ]
      , HH.section [ css "section pt-6" ]
          [ HH.slot_ (Proxy :: _ "walletView") unit WalletView.component unit ]
      ]
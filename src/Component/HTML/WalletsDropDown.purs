module Frontend.Component.HTML.WalletsDropDown (Message(..), component) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HA
import Network.RemoteData (RemoteData(..))

import Frontend.Capability.Domain.Browser (class ManageBrowser, disableWallet, enableWallet, getWallets)
import Frontend.Capability.LogMessages (class LogMessages, logMessage)
import Frontend.Component.HTML.Utils (css, delayAction, spinner, whenElem)
import Frontend.Data.Wallet (WalletId)

type State =
  { remoteData :: RemoteData InCaseOfNoWallets Choice
  , isDropDownActive :: Boolean
  }

data InCaseOfNoWallets
  = Hint
  | NoHint

derive instance eqInCaseOfNoWallets :: Eq InCaseOfNoWallets

type Choice =
  { wallets :: NonEmptyArray Wallet
  , selected :: Maybe Wallet
  }

type Wallet =
  { id :: WalletId
  , name :: String
  , apiVersion :: String
  , icon :: String
  }

data Action
  = Initialize
  | FindWallets
  | ToggleMenu
  | SelectWallet Wallet
  | DeselectWallet
  | RaiseReloadWallet
  | ShowHint
  | HideHint

type Output = Message

data Message = ReloadWallet

component
  :: ∀ query input m
   . MonadAff m
  => ManageBrowser m
  => LogMessages m
  => H.Component query input Output m
component =
  H.mkComponent
    { initialState: const { remoteData: NotAsked, isDropDownActive: false }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Initialize -> do
      H.modify_ \state -> state { remoteData = Loading }
      _ <- H.fork $ delayAction (Milliseconds 1000.0) handleAction FindWallets
      pure unit

    FindWallets -> do
      mbWallets <- getWallets
      let
        result = case mbWallets of
          Just wallets -> Success { wallets: wallets, selected: Nothing }
          _ -> Failure Hint
      H.modify_ \state -> state { remoteData = result }

    HideHint -> do
      H.modify_ \state -> state { remoteData = Failure NoHint }

    ShowHint ->
      H.modify_ \state -> state { remoteData = Failure Hint }

    ToggleMenu ->
      H.modify_ \state -> state { isDropDownActive = not state.isDropDownActive }

    SelectWallet wallet -> do
      H.modify_ \state -> state { isDropDownActive = false }
      logMessage $ "Wallet selected: " <> show wallet.id
      isWalletEnabled <- enableWallet wallet.id
      when isWalletEnabled $ H.modify_ \state -> case state.remoteData of
        Success choice -> state { remoteData = Success choice { selected = Just wallet } }
        _ -> state
      pure unit

    DeselectWallet -> do
      H.modify_ \state -> state { isDropDownActive = false }
      logMessage "Wallet deselected"
      H.modify_ \state -> case state.remoteData of
        Success choice -> state { remoteData = Success choice { selected = Nothing } }
        _ -> state
      disableWallet

    RaiseReloadWallet -> do
      H.modify_ \state -> state { isDropDownActive = false }
      H.raise ReloadWallet

  render :: State -> H.ComponentHTML Action () m
  render { remoteData: NotAsked } =
    HH.div [ css "navbar-item" ]
      [ HH.div_
          [ HH.text "Wallets" ]
      ]

  render { remoteData: Loading } =
    HH.div [ css "navbar-item" ]
      [ HH.div [ css "navbar-item" ]
          [ spinner
          , HH.span [ css "pl-2 pr-4" ]
              [ HH.text "Identifying wallets…" ]
          ]
      ]

  render { remoteData: (Failure toDo) } =
    HH.div [ css "navbar-item" ]
      [ HH.div [ css "navbar-item is-clickable", HE.onClick \_ -> ShowHint ]
          [ HH.span [ css "pr-4" ]
              [ HH.text "No wallets available" ]
          ]
      , whenElem (toDo == Hint) \_ -> renderHint
      ]

  render { remoteData: (Success { wallets, selected: Nothing }), isDropDownActive } =
    HH.div [ css ("navbar-item has-dropdown" <> if isDropDownActive then " is-active" else "") ]
      [ HH.div [ css "navbar-item is-clickable", HE.onClick \_ -> ToggleMenu ]
          [ HH.span_
              [ HH.text "Wallets" ]
          , HH.span [ css "icon is-small pl-4 pr-4" ]
              [ HH.i [ css ("fas " <> if isDropDownActive then "fa-angle-up" else "fa-angle-down") {-, aria-hidden="true" -} ] [] ]
          ]
      , HH.div [ css "navbar-dropdown is-right" ]
          (renderDropDownItem <$> toArray wallets)
      ]

  render { remoteData: (Success { wallets: _, selected: Just wallet }), isDropDownActive } =
    HH.div [ css ("navbar-item has-dropdown" <> if isDropDownActive then " is-active" else "") ]
      [ HH.div [ css "navbar-item is-clickable", HE.onClick \_ -> ToggleMenu ]
          [ HH.span [ css "icon is-small" ]
              [ HH.img [ HP.src wallet.icon ] ]
          , HH.span [ css "pl-2" ]
              [ HH.text wallet.name ]
          , HH.span [ css "icon is-small pl-4 pr-4" ]
              [ HH.i [ css ("fas " <> if isDropDownActive then "fa-angle-up" else "fa-angle-down") {-, aria-hidden="true" -} ] [] ]
          ]
      , HH.div [ css "navbar-dropdown is-right" ]
          [ HH.a [ css "navbar-item pr-4", HE.onClick \_ -> RaiseReloadWallet ]
              [ HH.div [ css "is-align-items-center is-flex" ]
                  [ HH.span [ css "icon is-small" ]
                      [ HH.i [ css "fas fa-refresh" ] [] ]
                  , HH.span [ css "pl-2" ]
                      [ HH.text "Reload" ]
                  ]
              ]
          , HH.hr [ css "dropdown-divider" ]
          , HH.a [ css "navbar-item pr-4", HE.onClick \_ -> DeselectWallet ]
              [ HH.div [ css "is-align-items-center is-flex" ]
                  [ HH.span [ css "icon is-small" ]
                      [ HH.i [ css "fas fa-xmark" ] [] ]
                  , HH.span [ css "pl-2" ]
                      [ HH.text "Disconnect" ]
                  ]
              ]
          ]
      ]

  renderDropDownItem :: Wallet -> H.ComponentHTML Action () m
  renderDropDownItem wallet =
    HH.a [ css "navbar-item", HE.onClick \_ -> SelectWallet wallet ]
      [ HH.div [ css "is-align-items-center is-flex" ]
          [ HH.span [ css "icon is-small" ]
              [ HH.img [ HP.src wallet.icon ] ]
          , HH.span [ css "pl-2" ]
              [ HH.text wallet.name ]
          ]
      ]

  renderHint :: H.ComponentHTML Action () m
  renderHint =
    HH.div [ css "modal is-active" ]
      [ HH.div [ css "modal-background" ]
          []
      , HH.div [ css "modal-card" ]
          [ HH.header [ css "modal-card-head" ]
              [ HH.p [ css "modal-card-title" ]
                  [ HH.text "No wallets available" ]
              , HH.button [ css "delete", HA.label "close", HE.onClick \_ -> HideHint ] []
              ]
          , HH.section [ css "modal-card-body" ]
              [ HH.div [ css "content" ]
                  [ HH.p_
                      [ HH.text "Please install a Cardano wallet to your browser." ]
                  , HH.p_
                      [ HH.text "The wallet should support the "
                      , HH.a [ HP.href "https://cips.cardano.org/cips/cip30" ] [ HH.text "CIP 30 - Cardano dApp-Wallet Web Bridge" ]
                      , HH.text " specification, as for instance:"
                      , HH.ul_
                          [ HH.li_ [ HH.a [ HP.href "https://eternl.io" ] [ HH.text "Eternl" ] ]
                          , HH.li_ [ HH.a [ HP.href "https://flint-wallet.com" ] [ HH.text "Flint" ] ]
                          , HH.li_ [ HH.a [ HP.href "https://gerowallet.io" ] [ HH.text "GeroWallet" ] ]
                          , HH.li_ [ HH.a [ HP.href "https://namiwallet.io" ] [ HH.text "Nami" ] ]
                          , HH.li_ [ HH.a [ HP.href "https://nu.fi" ] [ HH.text "NuFi" ] ]
                          , HH.li_ [ HH.a [ HP.href "https://typhonwallet.io" ] [ HH.text "Typhon" ] ]
                          , HH.li_ [ HH.a [ HP.href "https://yoroi-wallet.com" ] [ HH.text "Yoroi" ] ]
                          ]
                      ]
                  , HH.p_
                      [ HH.text "Furthermore, you can get test ADA from the "
                      , HH.a [ HP.href "https://docs.cardano.org/cardano-testnet/tools/faucet" ] [ HH.text "Cardano testnet faucets" ]
                      , HH.text " for testing purposes."
                      ]
                  ]
              ]
          ]
      ]

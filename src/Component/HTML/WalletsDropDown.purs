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

type State = RemoteData InCaseOfNoWallets Choice

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
    { initialState: const NotAsked
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
      H.put Loading
      _ <- H.fork $ delayAction (Milliseconds 1000.0) handleAction FindWallets
      pure unit

    FindWallets -> do
      mbWallets <- getWallets
      H.put $ case mbWallets of
        Just wallets -> Success { wallets: wallets, selected: Nothing }
        _ -> Failure Hint

    HideHint -> do
      H.put $ Failure NoHint

    ShowHint -> do
      H.put $ Failure Hint

    SelectWallet wallet -> do
      logMessage $ "Wallet selected: " <> show wallet.id
      isWalletEnabled <- enableWallet wallet.id
      when isWalletEnabled $ H.modify_ \state -> case state of
        Success choice -> Success choice { selected = Just wallet }
        _ -> state
      pure unit

    DeselectWallet -> do
      logMessage "Wallet deselected"
      H.modify_ \state -> case state of
        Success choice -> Success choice { selected = Nothing }
        _ -> state
      disableWallet

    RaiseReloadWallet -> do
      H.raise ReloadWallet

  render :: State -> H.ComponentHTML Action () m
  render NotAsked =
    HH.div [ css "navbar-item" ]
      [ HH.div_
          [ HH.text "Wallets" ]
      ]

  render Loading =
    HH.div [ css "navbar-item" ]
      [ HH.div [ css "navbar-link" ]
          [ spinner
          , HH.small [ css "pl-2" ]
              [ HH.text "Identifying wallets…" ]
          ]
      ]

  render (Failure toDo) =
    HH.div [ css "navbar-item" ]
      [ HH.div [ css "navbar-link", HE.onClick \_ -> ShowHint ]
          [ HH.text "No wallets available" ]
      , whenElem (toDo == Hint) \_ -> renderHint
      ]

  render (Success { wallets, selected: Nothing }) =
    HH.div [ css "navbar-item has-dropdown is-hoverable" ]
      [ HH.div [ css "navbar-link" ]
          [ HH.text "Wallets" ]
      , HH.div [ css "navbar-dropdown is-right" ]
          (renderDropDownItem <$> toArray wallets)
      ]

  render (Success { wallets: _, selected: Just wallet }) =
    HH.div [ css "navbar-item has-dropdown is-hoverable" ]
      [ HH.div [ css "navbar-link" ]
          [ HH.span [ css "icon is-small" ]
              [ HH.img [ HP.src wallet.icon ] ]
          , HH.small [ css "pl-2" ]
              [ HH.text wallet.name ]
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

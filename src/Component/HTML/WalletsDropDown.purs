module Frontend.Component.HTML.WalletsDropDown (Message(..), component) where

import Prelude

import Cardano.Wallet (WalletName)
import Data.Array (null)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HA
import Halogen.Store.Monad (class MonadStore, updateStore)
import Network.RemoteData (RemoteData(..))

import Frontend.Api.WalletName (unwrap) as WalletName
import Frontend.Capability.LogMessages (class LogMessages, logMessage)
import Frontend.Capability.Resource.Wallet (class ManageWallet, enableWallet)
import Frontend.Capability.Resource.WebPage (class ManageWebPage, availableWallets)
import Frontend.Component.HTML.Utils (css, whenElem)
import Frontend.Store (Action(..), Store) as Store

type State = RemoteData InCaseOfNoWallets Choice

data InCaseOfNoWallets
  = VisibleHint
  | HiddenHint

derive instance eqInCaseOfNoWallets :: Eq InCaseOfNoWallets

type Choice =
  { wallets :: Array Wallet -- TODO: non empty ...
  , selected :: Maybe Wallet
  }

type Wallet =
  { id :: WalletName
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
  => LogMessages m
  => MonadStore Store.Action Store.Store m
  => ManageWebPage m
  => ManageWallet m
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
      _ <- H.fork $ delayAction FindWallets $ Milliseconds 1000.0
      pure unit

    FindWallets -> do
      mbWallets <- H.lift $ availableWallets
      case mbWallets of
        Just wallets ->
          if not (null wallets) then H.put $ Success { wallets: wallets, selected: Nothing }
          else H.put $ Failure VisibleHint
        Nothing -> H.put $ Failure VisibleHint

    HideHint -> do
      H.put $ Failure HiddenHint

    ShowHint -> do
      H.put $ Failure VisibleHint

    SelectWallet wallet -> do
      logMessage $ "Wallet selected: " <> WalletName.unwrap wallet.id
      mbApi <- H.lift $ enableWallet wallet.id
      for_ mbApi \api -> do
        updateStore $ Store.EnableWallet { name: wallet.id, api: api }
        H.modify_ \state -> case state of
          Success choice -> Success choice { selected = Just wallet }
          _ -> state

    DeselectWallet -> do
      logMessage "Wallet deselected"
      H.modify_ \state -> case state of
        Success choice -> Success choice { selected = Nothing }
        _ -> state
      updateStore $ Store.DisableWallet

    RaiseReloadWallet -> do
      H.raise ReloadWallet

  -- FIXME: Move to capabilities to get rid of the type class constraint MonadAff 
  delayAction :: Action -> Milliseconds -> H.HalogenM State Action () Output m Unit
  delayAction action ms = do
    H.liftAff $ Aff.delay ms
    handleAction action

  render :: State -> H.ComponentHTML Action () m
  render NotAsked =
    HH.div [ css "navbar-item" ]
      [ HH.div_
          [ HH.text "Wallets" ]
      ]

  render Loading =
    HH.div [ css "navbar-item" ]
      [ HH.div [ css "navbar-link" ]
          [ HH.span [ css "icon is-small" ]
              [ HH.i [ css "fa fa-solid fa-spinner fa-spin" ] [] ]
          , HH.small [ css "pl-2" ]
              [ HH.text "Identifying wallets ..." ]
          ]
      ]

  render (Failure toDo) =
    HH.div [ css "navbar-item" ]
      [ HH.div [ css "navbar-link", HE.onClick \_ -> ShowHint ]
          [ HH.text "No wallets available" ]
      , whenElem (toDo == VisibleHint) \_ -> renderInstallWalletHint
      ]

  render (Success { wallets, selected: Nothing }) =
    HH.div [ css "navbar-item has-dropdown is-hoverable" ]
      [ HH.div [ css "navbar-link" ]
          [ HH.text "Wallets" ]
      , HH.div [ css "navbar-dropdown is-right" ]
          (renderDropDownItem <$> wallets)
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
                      [ HH.i [ css "fa fa-refresh" ] [] ]
                  , HH.span [ css "pl-2" ]
                      [ HH.text "Reload" ]
                  ]
              ]
          , HH.hr [ css "dropdown-divider" ]
          , HH.a [ css "navbar-item pr-4", HE.onClick \_ -> DeselectWallet ]
              [ HH.div [ css "is-align-items-center is-flex" ]
                  [ HH.span [ css "icon is-small" ]
                      [ HH.i [ css "fa fa-xmark" ] [] ]
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

  renderInstallWalletHint :: H.ComponentHTML Action () m
  renderInstallWalletHint =
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
                      [ HH.text "You can get test ADA from the "
                      , HH.a [ HP.href "https://docs.cardano.org/cardano-testnet/tools/faucet" ] [ HH.text "Cardano testnet faucets" ]
                      , HH.text "."
                      ]
                  ]
              ]
          ]
      ]
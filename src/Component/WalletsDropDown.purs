module Example.Component.WalletsDropDown (component) where

import Prelude

import Cardano.Wallet (WalletName)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)

import Example.Api.WalletName (unwrap) as WalletName
import Example.Capability.Resource.Wallet (class ManageWallet, enableWallet)
import Example.Capability.Resource.WebPage (class ManageWebPage, availableWallets)
import Example.Component.Utils (css)
import Example.Store (Action(..), Store) as Store

type State = Maybe Choice

type Choice = 
  { wallets :: Array Wallet
  , selected :: Maybe Wallet
  }

type Wallet =
  { id :: WalletName
  , name :: String
  , apiVersion :: String
  , icon :: String
  }

data Action = Initialize | FindWallets | SelectWallet Wallet | DeselectWallet

component
  :: ∀ query input output m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageWebPage m
  => ManageWallet m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState: const Nothing
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where

    render :: State -> H.ComponentHTML Action () m
    render Nothing =
      HH.div [ css "navbar-item" ]
        [ HH.div [ css "navbar-link" ]
            [ HH.span [ css "icon is-small" ] 
                [ HH.i [ css "fa fa-solid fa-spinner fa-spin" ] [] ] 
            , HH.small [ css "pl-2" ]
                [ HH.text "Identifying wallets ..."]
            ]
        ]

    render (Just { wallets, selected: Nothing }) =
        HH.div [ css "navbar-item has-dropdown is-hoverable" ]
          [ HH.div [ css "navbar-link" ]
              [ HH.text "Connect wallet ..."]
          , HH.div [ css "navbar-dropdown is-right" ]
              ( renderDropDownItem <$> wallets )
          ]

    render (Just { wallets: _, selected: Just wallet }) =
      HH.div [ css "navbar-item has-dropdown is-hoverable" ]
        [ HH.div [ css "navbar-link" ]
            [ HH.span [ css "icon is-small" ] 
                [ HH.img [ HP.src wallet.icon ] ]
            , HH.small [ css "pl-2" ]
                [ HH.text wallet.name ]
            ]
        , HH.div [ css "navbar-dropdown is-right" ]
            [ HH.a [ css "navbar-item pr-4" {-, HE.onClick \_ -> ReloadWallet -}]
                [ HH.div [ css "is-align-items-center is-flex" ]
                    [ HH.span [ css "icon is-small" ]
                        [ HH.i [ css "fa fa-refresh" ] [] ]
                    , HH.span [ css "pl-2" ]
                        [ HH.text "Reload"]
                    ]
                ]
            , HH.hr [ css "dropdown-divider" ]
            , HH.a [ css "navbar-item pr-4", HE.onClick \_ -> DeselectWallet ]
                [ HH.div [ css "is-align-items-center is-flex" ]
                    [ HH.span [ css "icon is-small" ]
                        [ HH.i [ css "fa fa-xmark" ] [] ]
                    , HH.span [ css "pl-2" ]
                        [ HH.text "Disconnect"]
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

    handleAction :: Action -> H.HalogenM State Action () output m Unit
    handleAction = case _ of
      Initialize -> do
        _ <- H.fork $ delayAction FindWallets $ Milliseconds 1000.0
        pure unit

      FindWallets -> do
        availableWallets <- H.lift $ availableWallets
        case availableWallets of
          Just wallets -> H.modify_ \_ -> Just { wallets: wallets, selected: Nothing } 
          _ -> log "Failed to determine available wallets"

      SelectWallet wallet -> do
        log $ "Wallet selected: " <> WalletName.unwrap wallet.id
        enabledWallet <- H.lift $ runExceptT $ enableWallet wallet.id
        case enabledWallet of
          Right api -> do
            updateStore $ Store.EnableWallet { name: wallet.id, api: api }
            H.modify_ \state -> (\choice -> choice { selected = Just wallet }) <$> state
          Left error -> do
            log $ "Failed to enable wallet " <> WalletName.unwrap wallet.id <> ": " <> show error

      DeselectWallet -> do
        log "Wallet deselected"
        H.modify_ \state -> (\choice -> choice { selected = Nothing }) <$> state
        updateStore $ Store.DisableWallet
        pure unit

    -- TODO: Extract to effectful type class
    delayAction :: Action -> Milliseconds -> H.HalogenM State Action () output m Unit
    delayAction action ms = do
      H.liftAff $ Aff.delay ms
      handleAction action
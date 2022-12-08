module Qapla.Component.WalletsDropDown (Output(..), component) where

import Prelude

import Cardano.Wallet (WalletName)
import Cardano.Wallet (getApiVersion, getName, getIcon, availableWallets) as CW
import Data.Array (filter, intercalate)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Qapla.Api.WalletName (unwrap) as WalletName
import Qapla.Component.Utils (css)

type Output = Maybe WalletName

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

component :: ∀ query m. MonadAff m => H.Component query Unit Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: ∀ input. input -> State
initialState _ = Nothing

render :: ∀ m. State -> H.ComponentHTML Action () m
render Nothing =
  HH.div [ css "navbar-item" ]
    [ HH.div [ css "navbar-link" ]
        [ HH.span [ css "icon is-small" ] 
            [ HH.i [ css "fa fa-solid fa-spinner fa-spin" ] [] ] 
        , HH.small [ css "pl-1" ]
            [ HH.text "Identifying wallets ..."]
        ]
    ]

render (Just { wallets, selected: Nothing }) =
    HH.div [ css "navbar-item has-dropdown is-hoverable" ]
      [ HH.div [ css "navbar-link" ]
          [ HH.small_
              [ HH.text "Connect wallet"]
          ]
      , HH.div [ css "navbar-dropdown" ]
          ( renderDropDownItem <$> wallets )
      ]
  where
    renderDropDownItem wallet =
        HH.a [ css "navbar-item", HE.onClick \_ -> SelectWallet wallet ]
          [ HH.div [ css "is-align-items-center is-flex" ]
              [ HH.span [ css "icon is-small" ]
                  [ HH.img [ HP.src wallet.icon ] ]
              , HH.span [ css "pl-1" ] 
                  [ HH.text wallet.name ]
              ]
          ]

render (Just { wallets: _, selected: Just wallet }) =
  HH.div [ css "navbar-item has-dropdown is-hoverable" ]
    [ HH.div [ css "navbar-link" ]
        [ HH.span [ css "icon is-small" ] 
            [ HH.img [ HP.src wallet.icon ] ]
        , HH.small [ css "pl-1" ]
            [ HH.text wallet.name ]
        ]
    , HH.div [ css "navbar-dropdown" ]
        [ HH.a [ css "navbar-item", HE.onClick \_ -> DeselectWallet ]
            [ HH.div [ css "is-align-items-center is-flex" ]
                [ HH.span [ css "icon is-small" ]
                    [ HH.i [ css "fa fa-solid fa-xmark" ] [] ]
                , HH.span [ css "pl-1" ]
                    [ HH.text "Disconnect wallet"]
                ]
            ]
        ]
    ]

handleAction :: ∀ m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.fork $ delayAction FindWallets $ Milliseconds 1000.0
    pure unit

  FindWallets -> do
    walletNames <- H.liftEffect $ CW.availableWallets
    log $ "Wallets found: " <> intercalate ", " (WalletName.unwrap <$> walletNames)
    wallets <- H.liftEffect $ traverse mkWallet $ filter (\walletName -> WalletName.unwrap walletName /= "ccvault") walletNames
    H.modify_ \_ -> Just { wallets: wallets, selected: Nothing }
    pure unit

  SelectWallet wallet -> do
    log $ "Wallet selected: " <> WalletName.unwrap wallet.id
    H.modify_ \state -> (\choice -> choice { selected = Just wallet }) <$> state
    H.raise $ Just wallet.id
    pure unit

  DeselectWallet -> do
    log "Wallet deselected"
    H.modify_ \state -> (\choice -> choice { selected = Nothing }) <$> state
    H.raise $ Nothing
    pure unit

delayAction :: ∀ m. MonadAff m => Action -> Milliseconds -> H.HalogenM State Action () Output m Unit
delayAction action ms = do
  H.liftAff $ Aff.delay ms
  handleAction action

mkWallet :: WalletName -> Effect Wallet
mkWallet walletName = do
  apiVersion <- CW.getApiVersion walletName
  name <- CW.getName walletName
  icon <- CW.getIcon walletName
  pure
    { id: walletName
    , name: name
    , apiVersion: apiVersion
    , icon: icon
    }
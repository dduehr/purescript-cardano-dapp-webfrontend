module Wallets
  ( Output(..)
  , component
  , tag
  )
  where

import Prelude (Unit, unit, bind, discard, pure, ($), (<>), (<$>), (/=))

import Data.Array (filter, intercalate)
import Data.Maybe(Maybe(..))
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

import Cardano.Wallet (WalletName(..))
import Cardano.Wallet (apiVersion, name, icon, availableWallets) as CW

data Output = WalletSelected WalletName

type State = Maybe (Array Wallet)

type Wallet = 
  { id :: WalletName
  , name :: String
  , apiVersion :: String
  , icon :: String
  }

data Action = Initialize | FindWallets | SelectWallet WalletName

component :: forall query m. MonadAff m => H.Component query Unit Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
    }

initialState :: forall input. input -> State
initialState _ = Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render Nothing =
  HH.div_
    [ HH.text "No wallets available" ]

render (Just wallets) =
  HH.div_
    (renderWallet <$> wallets)
  where
    renderWallet wallet =
      let
        walletId = tag wallet.id
      in
        HH.span_
          [ HH.input [ HP.type_ HP.InputRadio, HP.name "wallet", HP.id walletId, HP.value walletId, HE.onChange \_ -> SelectWallet wallet.id ] 
          , HH.label
            [ HP.for walletId ] 
            [ HH.img [ HP.src wallet.icon, HP.width 24, HP.height 24 ]
            , HH.text $ wallet.name <> " (" <> walletId <> ")"
            ]
          ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.fork $ delayAction FindWallets $ Milliseconds 1000.0 
    pure unit

  FindWallets -> do
    walletNames <- H.liftEffect $ CW.availableWallets
    wallets <- H.liftEffect $ traverse mkWallet $ filter (\walletName -> tag walletName /= "ccvault") walletNames
    _ <- H.modify \_ -> Just wallets
    log $ "wallets " <> intercalate ", " (tag <$> walletNames) <> " found"

  SelectWallet walletName -> do
    H.raise $ WalletSelected walletName
    log $ "wallet " <> tag walletName <> " selected"

delayAction :: forall m. MonadAff m => Action -> Milliseconds -> H.HalogenM State Action () Output m Unit
delayAction action ms = do
    H.liftAff $ Aff.delay ms
    handleAction action

mkWallet :: WalletName -> Effect Wallet
mkWallet walletName = do
  apiVersion <- CW.apiVersion walletName
  name <- CW.name walletName
  icon <- CW.icon walletName
  pure $ { id: walletName, name: name, apiVersion: apiVersion, icon: icon }

tag :: WalletName -> String
tag (WalletName _tag) = _tag
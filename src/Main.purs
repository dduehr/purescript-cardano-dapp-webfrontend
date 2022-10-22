module Main
  ( Action(..)
  , State
  , Wallet
  , component
  , delayAction
  , getWallet
  , handleAction
  , main
  , render
  , tag
  )
  where

import Prelude (Unit, bind, discard, pure, unit, ($), (<>), (<$>), (/=))

import Data.Array (filter, intercalate)
import Data.Maybe(Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import Cardano.Wallet (WalletName(..))
import Cardano.Wallet (apiVersion, name, icon, availableWallets) as CW

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type Wallet = 
  { id :: WalletName
  , name :: String
  , apiVersion :: String
  , icon :: String
  }

type State = Maybe (Array Wallet)

data Action = Initialize | FindWallets | EnableWallet WalletName

component :: forall query input output m. MonadAff m => H.Component query input output m
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
    [ HH.text "No wallets" ]
render (Just wallets) =
  HH.div_
    (renderWallet <$> wallets)
  where
    renderWallet wallet =
      let
        walletId = tag wallet.id
      in
        HH.span_
          [ HH.input [ HP.type_ HP.InputRadio, HP.name "wallet", HP.id walletId, HP.value walletId, HE.onChange \_ -> EnableWallet wallet.id ] 
          , HH.label
            [ HP.for walletId ] 
            [ HH.img [ HP.src wallet.icon, HP.width 24, HP.height 24 ]
            , HH.text $ wallet.name <> " (" <> walletId <> ")"
            ]
          ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "initializing ..."
    _ <- H.fork $ delayAction FindWallets $ Milliseconds 1000.0 
    log "initialzed"

  FindWallets -> do
    log "finding wallets ..."
    walletNames <- H.liftEffect $ CW.availableWallets
    wallets <- H.liftEffect $ traverse getWallet $ filter (\walletName -> tag walletName /= "ccvault") walletNames
    _ <- H.modify \_ -> Just wallets
    log $ "wallets " <> intercalate ", " (tag <$> walletNames) <> " found"

  EnableWallet walletName -> do
    log $ "TODO: enable wallet " <> tag walletName

delayAction :: forall output m. MonadAff m => Action -> Milliseconds -> H.HalogenM State Action () output m Unit
delayAction action ms = do
    H.liftAff $ Aff.delay ms
    handleAction action

getWallet :: WalletName -> Effect Wallet
getWallet walletName = do
  apiVersion <- CW.apiVersion walletName
  name <- CW.name walletName
  icon <- CW.icon walletName
  pure $ { id: walletName, name: name, apiVersion: apiVersion, icon: icon }

tag :: WalletName -> String
tag (WalletName _tag) = _tag
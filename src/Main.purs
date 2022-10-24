module Main (main) where

import Prelude (Unit, ($), (<>), (>>=), bind, discard, const, pure, show, unit)

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))

import Cardano.Wallet (Api, Cbor, NetworkId, WalletName)
import Cardano.Wallet (getApiVersion, enable, getBalance, getChangeAddress, getName, 
  getNetworkId, getRewardAddresses, getUsedAddresses, getUtxos) as CW

import Wallets (Output(..), component, tag) as Wallets


type State = Maybe Wallet

type Wallet = 
  { id :: WalletName
  , name :: String
  , apiVersion :: String
  , api :: Api
  , networkId :: NetworkId
  , balance :: Cbor
  , changeAddress :: Cbor
  , rewardAddresses :: Maybe (Array Cbor)
  , usedAddresses :: Maybe (Array Cbor)
  , utxos :: Maybe (Array Cbor)
  }

data Action = HandleWallets Wallets.Output

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type Slots = ( wallets :: forall query. H.Slot query Wallets.Output Unit )

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: const Nothing
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div_
    [ HH.h1_ [ HH.text "Boilerplate DApp Connector to Wallet" ]
    , HH.div_
      [ HH.p_ [ HH.text "Select wallet:" ]
      , HH.slot (Proxy :: _ "wallets") unit Wallets.component unit HandleWallets 
      ]
    , renderWallet state
    ]

renderWallet :: forall widget input. Maybe Wallet -> HH.HTML widget input
renderWallet Nothing =
  HH.div_
    [ HH.text "No wallet enabled" ]
renderWallet (Just wallet) =
  HH.div_ 
    [ HH.div_
      [ HH.p_ [ HH.text $ "Wallet name: " <> wallet.name ]
      , HH.p_ [ HH.text $ "Wallet API Version: " <>  wallet.apiVersion ]
      , HH.p_ [ HH.text $ "Network Id (0 = testnet; 1 = mainnet): " <> show wallet.networkId ]
      ]
    , HH.div_
      [ HH.p_ [ HH.text "UTXOs:" ]
      , HH.ul_
        [ HH.li_ [ HH.text "TODO"] ]
      ]
    , HH.div_
      [ HH.p_ [ HH.text $ "Balance: " <> wallet.balance ]
      , HH.p_ [ HH.text $ "Change Address: " <> wallet.changeAddress ]
      , HH.p_ [ HH.text $ "Reward Addresses: ", renderRewardAddresses wallet.rewardAddresses ]
      , HH.p_ [ HH.text $ "Used Addresses: ", renderUsedAddresses wallet.usedAddresses ]
      ]
    ]

renderRewardAddresses :: forall widget input. Maybe (Array Cbor) -> HH.HTML widget input
renderRewardAddresses Nothing =
  HH.text "Loading ..."
renderRewardAddresses (Just rewardAddresses) =
  HH.text $ show rewardAddresses

renderUsedAddresses :: forall widget input. Maybe (Array Cbor) -> HH.HTML widget input
renderUsedAddresses Nothing =
  HH.text "Loading ..."
renderUsedAddresses (Just usedAddresses) =
  HH.text $ show usedAddresses

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  HandleWallets (Wallets.WalletSelected walletName) -> do
    _ <- H.modify \_ -> Nothing
    wallet <- H.liftAff $ enableWallet walletName
    _ <- H.modify \_ -> Just wallet
    log $ "wallet enabled: " <> Wallets.tag walletName
    _ <- H.fork getRewardAddresses
    _ <- H.fork getUsedAddresses
    _ <- H.fork getUtxos
    pure unit

getRewardAddresses :: forall output m. MonadAff m => H.HalogenM State Action Slots output m Unit
getRewardAddresses =
  H.get >>= case _ of
    Just wallet -> do
      rewardAddresses <- liftAff $ CW.getRewardAddresses wallet.api
      log $ "got reward addresses: " <> show rewardAddresses
      _ <- H.modify \_ -> Just $ wallet { rewardAddresses = Just rewardAddresses }
      pure unit
    _ ->
      pure unit

getUsedAddresses :: forall output m. MonadAff m => H.HalogenM State Action Slots output m Unit
getUsedAddresses =
  H.get >>= case _ of
    Just wallet -> do
      usedAddresses <- liftAff $ CW.getUsedAddresses wallet.api { limit: 10, page: 0 }
      log $ "got used addresses: " <> show usedAddresses
      _ <- H.modify \_ -> Just $ wallet { usedAddresses = Just usedAddresses }
      pure unit
    _ ->
      pure unit

getUtxos :: forall output m. MonadAff m => H.HalogenM State Action Slots output m Unit
getUtxos =
  H.get >>= case _ of
    Just wallet -> do
      utxos <- liftAff $ CW.getUtxos wallet.api Nothing
      log $ "got utxos: " <> show utxos
      _ <- H.modify \_ -> Just $ wallet { utxos = Just utxos }
      pure unit
    _ ->
      pure unit

enableWallet :: WalletName -> Aff Wallet
enableWallet walletName = do
  name <- liftEffect $ CW.getName walletName
  apiVersion <- liftEffect $ CW.getApiVersion walletName
  api <- CW.enable walletName
  networkId <- CW.getNetworkId api
  balance <- CW.getBalance api
  changeAddress <- CW.getChangeAddress api
  pure 
    { id: walletName
    , name: name
    , apiVersion: apiVersion
    , api: api
    , networkId: networkId
    , balance: balance
    , changeAddress: changeAddress
    , rewardAddresses: Nothing
    , usedAddresses: Nothing
    , utxos: Nothing
    }
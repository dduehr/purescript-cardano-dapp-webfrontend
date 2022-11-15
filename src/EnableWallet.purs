module EnableWallet (Output(..), component) where

import Prelude (Unit, bind, const, discard, map, pure, show, unit, ($), (<$>), (<>))

import Csl as Csl
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

import Cardano.Wallet (Api, Cbor, NetworkId, WalletName)
import Cardano.Wallet
  ( getApiVersion
  , enable
  , getBalance
  , getChangeAddress
  , getName
  , getNetworkId
  , getRewardAddresses
  , getUsedAddresses
  , getUtxos
  ) as CW

import SelectWallet (Output(..), component, tag) as SelectWallet

data Output = WalletEnabled Api

type State = Maybe Wallet

type Wallet =
  { id :: WalletName
  , name :: String
  , apiVersion :: String
  , api :: Api
  , networkId :: NetworkId
  , balance :: Maybe Csl.BigNum
  , changeAddress :: Cbor
  , rewardAddresses :: Maybe (Array Cbor)
  , usedAddresses :: Maybe (Array Cbor)
  , utxos :: Maybe (Array Csl.TxOut)
  }

data Action = HandleSelectWallet SelectWallet.Output

type Slots = (wallets :: ∀ query. H.Slot query SelectWallet.Output Unit)

component :: ∀ query input m. MonadAff m => H.Component query input Output m
component =
  H.mkComponent
    { initialState: const Nothing
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: ∀ m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div_
    [ HH.h1_ [ HH.text "Boilerplate DApp Connector to Wallet" ]
    , HH.div_
        [ HH.p_ [ HH.text "Select wallet:" ]
        , HH.slot (Proxy :: _ "wallets") unit SelectWallet.component unit HandleSelectWallet
        ]
    , renderWallet state
    ]

renderWallet :: ∀ widget input. Maybe Wallet -> HH.HTML widget input
renderWallet Nothing =
  HH.div_
    [ HH.text "No wallet enabled" ]
renderWallet (Just wallet) =
  HH.div_
    [ HH.div_
        [ HH.p_ [ HH.text $ "Wallet name: " <> wallet.name ]
        , HH.p_ [ HH.text $ "Wallet API Version: " <> wallet.apiVersion ]
        , HH.p_ [ HH.text $ "Network Id (0 = testnet; 1 = mainnet): " <> show wallet.networkId ]
        ]
    , HH.div_
        [ HH.p_ [ HH.text "UTXOs:" ]
        , renderUtxos wallet.utxos
        ]
    , HH.div_
        [ HH.p_ [ HH.text $ "Balance: ", renderBalance wallet.balance ]
        , HH.p_ [ HH.text $ "Change Address: " <> wallet.changeAddress ]
        , HH.p_ [ HH.text $ "Reward Addresses: ", renderRewardAddresses wallet.rewardAddresses ]
        , HH.p_ [ HH.text $ "Used Addresses: ", renderUsedAddresses wallet.usedAddresses ]
        ]
    ]

renderUtxos :: ∀ widget input. Maybe (Array Csl.TxOut) -> HH.HTML widget input
renderUtxos Nothing =
  HH.text "Loading ..."
renderUtxos (Just utxos) =
  HH.ul_ $ (\utxo -> HH.li_ 
    [ HH.text $ Csl.address.toHex $ Csl.txOut.address utxo
    , HH.text ", "
    , HH.text $ Csl.bigNum.toStr $ Csl.value.coin $ Csl.txOut.amount utxo 
    ]) <$> utxos

renderBalance :: ∀ widget input. Maybe Csl.BigNum -> HH.HTML widget input
renderBalance Nothing =
  HH.text "?"
renderBalance (Just balance) =
  HH.text $ Csl.bigNum.toStr $ balance

renderRewardAddresses :: ∀ widget input. Maybe (Array Cbor) -> HH.HTML widget input
renderRewardAddresses Nothing =
  HH.text "Loading ..."
renderRewardAddresses (Just rewardAddresses) =
  HH.text $ show rewardAddresses

renderUsedAddresses :: ∀ widget input. Maybe (Array Cbor) -> HH.HTML widget input
renderUsedAddresses Nothing =
  HH.text "Loading ..."
renderUsedAddresses (Just usedAddresses) =
  HH.text $ show usedAddresses

handleAction :: ∀ m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  HandleSelectWallet (SelectWallet.WalletSelected walletName) -> do
    H.put Nothing
    wallet <- H.liftAff $ enableWallet walletName
    log $ "wallet enabled: " <> SelectWallet.tag walletName
    H.put $ Just wallet
    H.raise $ WalletEnabled wallet.api
    rewardAddresses <- liftAff $ CW.getRewardAddresses wallet.api
    log $ "got reward addresses: " <> show rewardAddresses
    _ <- H.modify \maybeWallet -> _ { rewardAddresses = Just rewardAddresses } <$> maybeWallet
    usedAddresses <- liftAff $ CW.getUsedAddresses wallet.api { limit: 10, page: 0 }
    log $ "got used addresses: " <> show usedAddresses
    _ <- H.modify \maybeWallet -> _ { usedAddresses = Just usedAddresses } <$> maybeWallet
    -- CW.getUtxos :: Api -> Maybe Paginate -> Aff (Array Cbor)
    utxos' <- liftAff $ CW.getUtxos wallet.api Nothing
    log $ "got utxos (cbor): " <> show utxos'
    -- Csl.fromHex :: ∀ a. TxUnspentOut a => String -> Maybe a
    -- Csl.txUnspentOut.out :: TxUnspentOut -> TxOut
    let utxos = sequence $ map Csl.txUnspentOut.out <$> Csl.fromHex <$> utxos' 

    log $ "fromHex utxos: " <> show utxos
    _ <- H.modify \maybeWallet -> _ { utxos = utxos } <$> maybeWallet
    pure unit

enableWallet :: WalletName -> Aff Wallet
enableWallet walletName = do
  name <- liftEffect $ CW.getName walletName
  apiVersion <- liftEffect $ CW.getApiVersion walletName
  api <- CW.enable walletName
  networkId <- CW.getNetworkId api
  balance <- Csl.fromHex <$> CW.getBalance api
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
module Qapla.Component.WalletView (component) where

import Prelude

import Cardano.Wallet (Api, NetworkId, WalletName)
import Cardano.Wallet (getApiVersion, enable, getBalance, getChangeAddress, getName, getNetworkId, getRewardAddresses, getUsedAddresses, getUtxos) as CW
import Csl (Address, BigNum, TxUnspentOut, address, bigNum, fromHex, txHash, txIn, txOut, txUnspentOut, value) as Csl
import Data.Int (floor)
import Data.Lens (Lens', Prism', prism', set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Qapla.Api.WalletName (unwrap) as WalletName
import Qapla.Component.Utils (css)
import Type.Proxy (Proxy(..))

data Action 
  = Receive Input
  | Reload

type Input = Maybe WalletName

data State 
  = Received Input
  | Loaded Wallet
  | Failed Message

type Wallet =
  { id :: WalletName
  , name :: String
  , apiVersion :: String
  , api :: Api
  , networkId :: NetworkId
  , balance :: Maybe Csl.BigNum
  , changeAddress :: Maybe Csl.Address
  , rewardAddresses :: Maybe (Array Csl.Address)
  , usedAddresses :: Maybe (Array Csl.Address)
  , utxos :: Maybe (Array Csl.TxUnspentOut)
  }

type Message = String

component :: ∀ query output m. MonadAff m => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }

initialState :: Input -> State
initialState = Received

render :: ∀ m. MonadAff m => State -> H.ComponentHTML Action () m
render (Received input) =
  renderInput input
render (Loaded wallet) =
  HH.div_
    [ HH.h2 [ css "title is-4" ]
      [ HH.text $ "Wallet" ]
    , renderWallet wallet 
    ]
render (Failed message) =
  HH.text message

renderInput :: ∀ w i. Input -> HH.HTML w i
renderInput Nothing = 
  HH.text ""
renderInput (Just walletName) =
  HH.span_
    [ HH.text "Wallet "
    , HH.text $ WalletName.unwrap walletName
    , HH.text " selected ..."
    ]

renderWallet :: ∀ w i. Wallet -> HH.HTML w i
renderWallet wallet =
  HH.table [ css "table is-fullwidth" ]
    [ HH.tr_
        [ HH.th_ [ HH.text "Name" ]
        , HH.td_ [ HH.text wallet.name ]
        ]
    , HH.tr_
        [ HH.th_ [ HH.text "API Version" ]
        , HH.td_ [ HH.text wallet.apiVersion ]
        ]
    , HH.tr_
        [ HH.th_ [ HH.text "Network Id" ]
        , HH.td_ [ HH.text $ show wallet.networkId ]
        ]
    , HH.tr_
        [ HH.th_ [ HH.text "Balance" ]
        , HH.td_ [ renderBalance wallet.balance ]
        ]
    , HH.tr_
        [ HH.th_ [ HH.text "UTxOs" ]
        , HH.td_ [ renderUtxos wallet.utxos ]
        ]
    , HH.tr_
        [ HH.th_ [ HH.text "Change Addresses" ]
        , HH.td_ [ renderChangeAddress wallet.changeAddress ]
        ]
    , HH.tr_
        [ HH.th_ [ HH.text "Reward Addresses" ]
        , HH.td_ [ renderRewardAddresses wallet.rewardAddresses ]
        ]
    , HH.tr_
        [ HH.th_ [ HH.text "Used Addresses" ]
        , HH.td_ [ renderUsedAddresses wallet.usedAddresses ]
        ]
    ]

renderBalance :: ∀ w i. Maybe Csl.BigNum -> HH.HTML w i
renderBalance Nothing = renderSpinner
renderBalance (Just balance) = HH.text $ Csl.bigNum.toStr balance

renderUtxos :: ∀ w i. Maybe (Array Csl.TxUnspentOut) -> HH.HTML w i
renderUtxos Nothing = renderSpinner
renderUtxos (Just utxos) =
  HH.div [ css "list" ]
    [ HH.ul_ $ renderUtxo <$> utxos ]

renderUtxo :: ∀ w i. Csl.TxUnspentOut -> HH.HTML w i
renderUtxo utxo =
  HH.div [ css "list-item" ]
    [ HH.li_
      [ HH.text $ Csl.txHash.toHex $ Csl.txIn.txId $ Csl.txUnspentOut.in utxo
      , HH.text " #"
      , HH.text $ show $ floor $ Csl.txIn.index $ Csl.txUnspentOut.in utxo
      , HH.text " = "
      , HH.text $ Csl.bigNum.toStr $ Csl.value.coin $ Csl.txOut.amount  $ Csl.txUnspentOut.out utxo
      ]
    ]

renderChangeAddress :: ∀ w i. Maybe (Csl.Address) -> HH.HTML w i
renderChangeAddress Nothing = renderSpinner
renderChangeAddress (Just changeAddress) = HH.text $ Csl.address.toBech32 changeAddress Nothing

renderRewardAddresses :: ∀ w i. Maybe (Array Csl.Address) -> HH.HTML w i
renderRewardAddresses Nothing = renderSpinner
renderRewardAddresses (Just rewardAddresses) =
  HH.div [ css "list" ]
    [ HH.ul_ $ renderAddress <$> rewardAddresses ]

renderUsedAddresses :: ∀ w i. Maybe (Array Csl.Address) -> HH.HTML w i
renderUsedAddresses Nothing = renderSpinner
renderUsedAddresses (Just usedAddresses) =
  HH.div [ css "list" ]
    [ HH.ul_ $ renderAddress <$> usedAddresses ]

renderAddress :: ∀ w i. Csl.Address -> HH.HTML w i
renderAddress address =
  HH.div [ css "list-item" ]
    [ HH.li_
      [ HH.text $ Csl.address.toBech32 address Nothing ]
    ]

renderSpinner :: ∀ w i. HH.HTML w i
renderSpinner =
  HH.span [ css "icon is-small" ]
    [ HH.i [ css "fa fa-solid fa-spinner fa-spin" ] [] ]

handleAction :: ∀ output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Receive Nothing ->
    H.put $ Received Nothing
  Receive (Just walletName) -> do
    name <- liftEffect $ CW.getName walletName
    apiVersion <- liftEffect $ CW.getApiVersion walletName
    api <- liftAff $ CW.enable walletName
    networkId <- liftAff $ CW.getNetworkId api
    H.put $ Loaded
      { id: walletName
      , name: name
      , apiVersion: apiVersion
      , api: api
      , networkId: networkId
      , balance: Nothing
      , changeAddress: Nothing
      , rewardAddresses: Nothing
      , usedAddresses: Nothing
      , utxos: Nothing
      } 
    handleAction Reload
  Reload -> do
    state <- H.get
    case state of
      (Loaded wallet) -> do
        balance <- H.liftAff $ Csl.fromHex <$> CW.getBalance wallet.api 
        H.modify_ \state' -> set (_Loaded <<< _balance) balance state'
        changeAddress <- H.liftAff $ Csl.fromHex <$> CW.getChangeAddress wallet.api
        H.modify_ \state' -> set (_Loaded <<< _changeAddress) changeAddress state'
        rewardAddresses <- liftAff $ traverse Csl.address.fromHex <$> CW.getRewardAddresses wallet.api
        H.modify_ \state' -> set (_Loaded <<< _rewardAddresses) rewardAddresses state'
        usedAddresses <- liftAff $ traverse Csl.address.fromHex <$> CW.getUsedAddresses wallet.api { limit: 10, page: 0 }
        H.modify_ \state' -> set (_Loaded <<< _usedAddresses) usedAddresses state'
        utxos <- liftAff $ traverse Csl.txUnspentOut.fromHex <$> CW.getUtxos wallet.api Nothing
        H.modify_ \state' -> set (_Loaded <<< _utxos) utxos state' 
      _ -> pure unit

_Loaded :: Prism' State Wallet 
_Loaded = prism' Loaded case _ of
  Loaded wallet -> Just wallet
  _ -> Nothing

_balance :: Lens' Wallet (Maybe Csl.BigNum)
_balance = prop (Proxy :: Proxy "balance")

_changeAddress :: Lens' Wallet (Maybe Csl.Address)
_changeAddress = prop (Proxy :: Proxy "changeAddress")

_rewardAddresses :: Lens' Wallet (Maybe (Array Csl.Address))
_rewardAddresses = prop (Proxy :: Proxy "rewardAddresses")

_usedAddresses :: Lens' Wallet (Maybe (Array Csl.Address))
_usedAddresses = prop (Proxy :: Proxy "usedAddresses")

_utxos :: Lens' Wallet (Maybe (Array Csl.TxUnspentOut))
_utxos = prop (Proxy :: Proxy "utxos")
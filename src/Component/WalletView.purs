module Example.Component.WalletView (component) where

import Prelude

import Cardano.Wallet (Api, NetworkId, WalletName)
import Cardano.Wallet (getApiVersion, getBalance, getChangeAddress, getName, getNetworkId, getRewardAddresses, getUsedAddresses, getUtxos) as CW
import Csl (Address, BigNum, TxUnspentOut, address, bigNum, fromHex, txHash, txIn, txOut, txUnspentOut, value) as Csl
import Data.Array.NonEmpty (fromArray, head)
import Data.Int (floor)
import Data.Lens (Lens', Prism', prism', set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (Selector, select)
import Type.Proxy (Proxy(..))

import Example.Api.WalletName (unwrap) as WalletName
import Example.Component.Utils (css)
import Example.Store (Action, Store, Wallet) as Store

type Input = Unit

type Context = Maybe Store.Wallet

data State 
  = Received Context
  | Loaded Wallet

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

data Action 
  = Receive (Connected Context Input)
  | Reload

component
  :: ∀ query output m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input output m
component =
  connect selectContext $ H.mkComponent
    { initialState: deriveState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }
  where

    deriveState :: Connected Context Input -> State
    deriveState { context: context } = Received context

    eqContext :: Context -> Context -> Boolean
    eqContext (Just walletA) (Just walletB) = WalletName.unwrap walletA.name == WalletName.unwrap walletB.name 
    eqContext Nothing Nothing = true
    eqContext _ _ = false

    selectContext :: Selector Store.Store Context
    selectContext = select eqContext \store -> store.wallet

    render :: State -> H.ComponentHTML Action () m
    render (Received context) =
      HH.div [ css "card p-4" ]
        [ HH.h2 [ css "title is-5 mb-3" ]
            [ HH.text "Wallet" ]
        , renderContext context
        ]

    render (Loaded wallet) =
      HH.div [ css "card p-4" ]
        [ HH.h2 [ css "title is-5 mb-3" ]
            [ HH.text "Wallet" ]
        , renderWallet wallet 
        ]

    renderContext :: ∀ w i. Context -> HH.HTML w i
    renderContext Nothing = 
      HH.text "No wallet connected"

    renderContext (Just wallet) =
      HH.span_
        [ HH.text "Connecting to wallet "
        , HH.text $ WalletName.unwrap wallet.name
        , HH.text " ..."
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
            [ HH.th_ [ HH.text "Change Address" ]
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
      case fromArray utxos of
        Nothing ->
          HH.div_ []
        Just nonEmptyUtxos -> 
          HH.div [ css "dropdown is-hoverable is-flex" ]
            [ HH.div [ css "dropdown-trigger is-fullwidth" ]
                [ HH.span [ {- aria-haspopup="true" aria-controls="dropdown-menu4" -}]
                    [ HH.text $ formatUtxo $ head nonEmptyUtxos ]
                , HH.text " "
                , HH.span [ css "icon is-small" ]
                    [ HH.i [ css "fas fa-angle-down" {-, aria-hidden="true" -} ] [] ]
                ]
            , HH.div [ css "dropdown-menu is-fullwidth" ]
                [ HH.div [ css "dropdown-content" ]
                    [ HH.div [ css "dropdown-item" ]
                        [ HH.div [ css "list" ]
                            [ HH.ul_
                                ( renderUtxo <$> utxos )
                            ]
                        ]
                    ]
                ]
            ]

    renderUtxo :: ∀ w i. Csl.TxUnspentOut -> HH.HTML w i
    renderUtxo utxo =
      HH.div [ css "list-item" ]
        [ HH.li_
          [ HH.text $ formatUtxo utxo ]
        ]

    formatUtxo :: Csl.TxUnspentOut -> String
    formatUtxo utxo = Csl.txHash.toHex (Csl.txIn.txId $ Csl.txUnspentOut.in utxo)
      <> " #"
      <> show (floor $ Csl.txIn.index $ Csl.txUnspentOut.in utxo)
      <> " "
      <> Csl.bigNum.toStr (Csl.value.coin $ Csl.txOut.amount $ Csl.txUnspentOut.out utxo)

    renderChangeAddress :: ∀ w i. Maybe (Csl.Address) -> HH.HTML w i
    renderChangeAddress Nothing = renderSpinner
    renderChangeAddress (Just changeAddress) = HH.text $ Csl.address.toBech32 changeAddress Nothing

    renderRewardAddresses :: ∀ w i. Maybe (Array Csl.Address) -> HH.HTML w i
    renderRewardAddresses Nothing = renderSpinner
    renderRewardAddresses (Just rewardAddresses) =
      case fromArray rewardAddresses of
        Nothing ->
          HH.div_ []
        Just nonEmptyRewardAddresses ->
          HH.div [ css "dropdown is-hoverable is-flex" ]
            [ HH.div [ css "dropdown-trigger is-fullwidth" ]
                [ HH.span [ {- aria-haspopup="true" aria-controls="dropdown-menu4" -}]
                    [ HH.text $ formatAddress $ head nonEmptyRewardAddresses ]
                , HH.text " "
                , HH.span [ css "icon is-small" ]
                    [ HH.i [ css "fas fa-angle-down" {-, aria-hidden="true" -} ] [] ]
                ]
            , HH.div [ css "dropdown-menu is-fullwidth" ]
                [ HH.div [ css "dropdown-content" ]
                    [ HH.div [ css "dropdown-item" ]
                        [ HH.div [ css "list" ]
                            [ HH.ul_
                                ( renderAddress <$> rewardAddresses )
                            ]
                        ]
                    ]
                ]
            ]

    renderUsedAddresses :: ∀ w i. Maybe (Array Csl.Address) -> HH.HTML w i
    renderUsedAddresses Nothing = renderSpinner
    renderUsedAddresses (Just usedAddresses) =
      case fromArray usedAddresses of
        Nothing ->
          HH.div_ []
        Just nonEmptyUsedAddresses ->
          HH.div [ css "dropdown is-hoverable is-flex" ]
            [ HH.div [ css "dropdown-trigger is-fullwidth" ]
                [ HH.span [ {- aria-haspopup="true" aria-controls="dropdown-menu4" -}]
                    [ HH.text $ formatAddress $ head nonEmptyUsedAddresses ]
                , HH.text " "
                , HH.span [ css "icon is-small" ]
                    [ HH.i [ css "fas fa-angle-down" {-, aria-hidden="true" -} ] [] ]
                ]
            , HH.div [ css "dropdown-menu is-fullwidth" ]
                [ HH.div [ css "dropdown-content" ]
                    [ HH.div [ css "dropdown-item" ]
                        [ HH.div [ css "list" ]
                            [ HH.ul_
                                ( renderAddress <$> usedAddresses )
                            ]
                        ]
                    ]
                ]
            ]

    renderAddress :: ∀ w i. Csl.Address -> HH.HTML w i
    renderAddress address =
      HH.div [ css "list-item" ]
        [ HH.li_
          [ HH.text $ formatAddress address ]
        ]

    formatAddress :: Csl.Address -> String
    formatAddress address =
      Csl.address.toBech32 address Nothing

    renderSpinner :: ∀ w i. HH.HTML w i
    renderSpinner =
      HH.span [ css "icon is-small" ]
        [ HH.i [ css "fa fa-solid fa-spinner fa-spin" ] [] ]

    handleAction :: Action -> H.HalogenM State Action () output m Unit
    handleAction = case _ of
      Receive { context: Nothing } ->
        H.put $ Received Nothing
      Receive { context: Just wallet } -> do
        H.put $ Received $ Just wallet
        name <- liftEffect $ CW.getName wallet.name
        apiVersion <- liftEffect $ CW.getApiVersion wallet.name
        networkId <- liftAff $ CW.getNetworkId wallet.api
        H.put $ Loaded
          { id: wallet.name
          , name: name
          , apiVersion: apiVersion
          , api: wallet.api
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
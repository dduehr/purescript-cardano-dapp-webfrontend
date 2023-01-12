module Frontend.Component.HTML.WalletView
  ( component
  , Query(..)
  ) where

import Prelude

import Cardano.Wallet as CW
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Csl as CS
import Data.Array.NonEmpty (fromArray, head)
import Data.Int (floor)
import Data.Lens (Lens', Prism', prism', set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (convertDuration)
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.HalogenM (ForkId, kill)
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, getStore)
import Halogen.Store.Select (Selector, select)
import Type.Proxy (Proxy(..))

import Frontend.Capability.Domain.Browser (class ManageBrowser, getWallet)
import Frontend.Capability.Domain.Wallet (class ManageWallet, getWalletNetworkId, getWalletBalance, getWalletChangeAddress, getWalletRewardAddresses, getWalletUsedAddresses, getWalletUtxos)
import Frontend.Component.HTML.Utils (css, repeatAction)
import Frontend.Data.Wallet (WalletApi, WalletCredentials, WalletId)
import Frontend.Store (Action, Store) as Store

type Input = Unit

data Query a = ReloadWallet a

type StoreContext = Maybe WalletCredentials

type WalletContext =
  { wallet :: Wallet
  , mbReloadSchedulerForkId :: Maybe ForkId
  }

data State
  = Received StoreContext
  | Loaded WalletContext

type Wallet =
  { id :: WalletId
  , name :: String
  , apiVersion :: String
  , api :: WalletApi
  , networkId :: CW.NetworkId
  , balance :: Maybe CS.BigNum
  , changeAddress :: Maybe CS.Address
  , rewardAddresses :: Maybe (Array CS.Address)
  , usedAddresses :: Maybe (Array CS.Address)
  , utxos :: Maybe (Array CS.TxUnspentOut)
  }

data Action
  = Receive (Connected StoreContext Input)
  | Reload

component
  :: ∀ output m
   . MonadAff m
  => ManageBrowser m
  => ManageWallet m
  => MonadStore Store.Action Store.Store m
  => H.Component Query Input output m
component =
  connect selectContext $ H.mkComponent
    { initialState: deriveState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        }
    }
  where

  selectContext :: Selector Store.Store StoreContext
  selectContext = select (\a b -> (_.id <$> a) == (_.id <$> b)) \store -> store.mbWalletCredentials

  deriveState :: Connected StoreContext Input -> State
  deriveState { context: mbWalletCredentials } = Received mbWalletCredentials

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Receive { context: Nothing } -> do
      killReloadScheduler
      H.put $ Received Nothing

    Receive { context: Just walletCredentials } -> do
      killReloadScheduler
      H.put $ Received (Just walletCredentials)
      mbWallet <- H.lift $ runMaybeT $ do
        wallet <- MaybeT $ getWallet walletCredentials.id
        networkId <- MaybeT $ getWalletNetworkId walletCredentials.api
        pure
          { id: walletCredentials.id
          , name: wallet.name
          , apiVersion: wallet.apiVersion
          , api: walletCredentials.api
          , networkId: networkId
          , balance: Nothing
          , changeAddress: Nothing
          , rewardAddresses: Nothing
          , usedAddresses: Nothing
          , utxos: Nothing
          }
      for_ mbWallet \wallet -> do
        { mbWalletReloadSec } <- getStore
        mbReloadSchedulerForkId <- case mbWalletReloadSec of
          Just seconds -> do
            forkId <- H.fork $ repeatAction (convertDuration seconds) handleAction Reload
            pure $ Just forkId
          Nothing ->
            pure Nothing
        H.put $ Loaded { wallet, mbReloadSchedulerForkId }
        handleAction Reload

    Reload -> do
      state <- H.get
      case state of
        Loaded { wallet } -> do
          -- FIXME: Code duplication
          H.modify_ \state' -> set (_Loaded <<< _wallet <<< _balance) Nothing state'
          balance <- getWalletBalance wallet.api
          H.modify_ \state' -> set (_Loaded <<< _wallet <<< _balance) balance state'

          H.modify_ \state' -> set (_Loaded <<< _wallet <<< _changeAddress) Nothing state'
          changeAddress <- getWalletChangeAddress wallet.api
          H.modify_ \state' -> set (_Loaded <<< _wallet <<< _changeAddress) changeAddress state'

          H.modify_ \state' -> set (_Loaded <<< _wallet <<< _rewardAddresses) Nothing state'
          rewardAddresses <- getWalletRewardAddresses wallet.api
          H.modify_ \state' -> set (_Loaded <<< _wallet <<< _rewardAddresses) rewardAddresses state'

          H.modify_ \state' -> set (_Loaded <<< _wallet <<< _usedAddresses) Nothing state'
          usedAddresses <- getWalletUsedAddresses wallet.api
          H.modify_ \state' -> set (_Loaded <<< _wallet <<< _usedAddresses) usedAddresses state'

          H.modify_ \state' -> set (_Loaded <<< _wallet <<< _utxos) Nothing state'
          utxos <- getWalletUtxos wallet.api
          H.modify_ \state' -> set (_Loaded <<< _wallet <<< _utxos) utxos state'

        _ -> pure unit

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action () output m (Maybe a)
  handleQuery = case _ of
    ReloadWallet a -> do
      _ <- handleAction Reload
      pure $ Just a

  killReloadScheduler :: H.HalogenM State Action () output m Unit
  killReloadScheduler = do
    state <- H.get
    case state of
      Loaded { mbReloadSchedulerForkId: Just forkId } -> kill forkId
      _ -> pure unit

  _Loaded :: Prism' State WalletContext
  _Loaded = prism' Loaded case _ of
    Loaded context -> Just context
    _ -> Nothing

  _wallet :: Lens' WalletContext Wallet
  _wallet = prop (Proxy :: Proxy "wallet")

  _balance :: Lens' Wallet (Maybe CS.BigNum)
  _balance = prop (Proxy :: Proxy "balance")

  _changeAddress :: Lens' Wallet (Maybe CS.Address)
  _changeAddress = prop (Proxy :: Proxy "changeAddress")

  _rewardAddresses :: Lens' Wallet (Maybe (Array CS.Address))
  _rewardAddresses = prop (Proxy :: Proxy "rewardAddresses")

  _usedAddresses :: Lens' Wallet (Maybe (Array CS.Address))
  _usedAddresses = prop (Proxy :: Proxy "usedAddresses")

  _utxos :: Lens' Wallet (Maybe (Array CS.TxUnspentOut))
  _utxos = prop (Proxy :: Proxy "utxos")

  render :: State -> H.ComponentHTML Action () m
  render (Received context) =
    HH.div [ css "card p-4" ]
      [ HH.h2 [ css "title is-5 mb-3" ]
          [ HH.text "Wallet" ]
      , renderContext context
      ]

  render (Loaded { wallet }) =
    HH.div [ css "card p-4" ]
      [ HH.h2 [ css "title is-5 mb-3" ]
          [ HH.text "Wallet" ]
      , renderWallet wallet
      ]

  renderContext :: ∀ w i. StoreContext -> HH.HTML w i
  renderContext Nothing =
    HH.text "No wallet connected"

  renderContext (Just wallet) =
    HH.span_
      [ HH.text "Connecting to wallet "
      , HH.text $ show wallet.id
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

  renderBalance :: ∀ w i. Maybe CS.BigNum -> HH.HTML w i
  renderBalance Nothing = renderSpinner
  renderBalance (Just balance) = HH.text $ CS.bigNum.toStr balance

  renderUtxos :: ∀ w i. Maybe (Array CS.TxUnspentOut) -> HH.HTML w i
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
                              (renderUtxo <$> utxos)
                          ]
                      ]
                  ]
              ]
          ]

  renderUtxo :: ∀ w i. CS.TxUnspentOut -> HH.HTML w i
  renderUtxo utxo =
    HH.div [ css "list-item" ]
      [ HH.li_
          [ HH.text $ formatUtxo utxo ]
      ]

  formatUtxo :: CS.TxUnspentOut -> String
  formatUtxo utxo = CS.txHash.toHex (CS.txIn.txId $ CS.txUnspentOut.in utxo)
    <> " #"
    <> show (floor $ CS.txIn.index $ CS.txUnspentOut.in utxo)
    <> " "
    <> CS.bigNum.toStr (CS.value.coin $ CS.txOut.amount $ CS.txUnspentOut.out utxo)

  renderChangeAddress :: ∀ w i. Maybe (CS.Address) -> HH.HTML w i
  renderChangeAddress Nothing = renderSpinner
  renderChangeAddress (Just changeAddress) = HH.text $ CS.address.toBech32 changeAddress Nothing

  renderRewardAddresses :: ∀ w i. Maybe (Array CS.Address) -> HH.HTML w i
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
                              (renderAddress <$> rewardAddresses)
                          ]
                      ]
                  ]
              ]
          ]

  renderUsedAddresses :: ∀ w i. Maybe (Array CS.Address) -> HH.HTML w i
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
                              (renderAddress <$> usedAddresses)
                          ]
                      ]
                  ]
              ]
          ]

  renderAddress :: ∀ w i. CS.Address -> HH.HTML w i
  renderAddress address =
    HH.div [ css "list-item" ]
      [ HH.li_
          [ HH.text $ formatAddress address ]
      ]

  formatAddress :: CS.Address -> String
  formatAddress address =
    CS.address.toBech32 address Nothing

  renderSpinner :: ∀ w i. HH.HTML w i
  renderSpinner =
    HH.span [ css "icon is-small" ]
      [ HH.i [ css "fas fa-solid fa-spinner fa-spin" ] [] ]

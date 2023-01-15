module Frontend.Component.HTML.WalletView
  ( component
  , Query(..)
  ) where

import Prelude

import Cardano.Wallet as CW
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Csl as CS
import Control.Bind (bindFlipped)
import Data.Array.NonEmpty (fromArray)
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

import Frontend.Api.Domain.Browser (class ManageBrowser, getWallet)
import Frontend.Api.Domain.Wallet (class ManageWallet, getWalletNetworkId, getWalletBalance, getWalletChangeAddress, getWalletRewardAddresses, getWalletUsedAddresses, getWalletUtxos)
import Frontend.Component.HTML.Utils (css, repeatAction)
import Frontend.Component.LoadText as LoadText
import Frontend.Component.LoadTexts as LoadTexts
import Frontend.Data.Wallet (WalletApi, WalletCredentials, WalletId)
import Frontend.Store (Action, Store) as Store

type Input = Unit

data Query a = ReloadWallet a

type Context = Maybe WalletCredentials

data State
  = Received Context
  | Loaded WalletContext

type WalletContext =
  { wallet :: Wallet
  , mbReloadSchedulerForkId :: Maybe ForkId
  , isDirtyToggle :: Boolean
  }

type Wallet =
  { id :: WalletId
  , name :: String
  , apiVersion :: String
  , api :: WalletApi
  , networkId :: CW.NetworkId
  }

data Action
  = Receive (Connected Context Input)
  | Reload

type Slots =
  ( loadText :: ∀ query output. H.Slot query output Int
  , loadTexts :: ∀ query output. H.Slot query output Int
  )

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

  selectContext :: Selector Store.Store Context
  selectContext = select (\a b -> (_.id <$> a) == (_.id <$> b)) \store -> store.mbWalletCredentials

  deriveState :: Connected Context Input -> State
  deriveState { context: mbWalletCredentials } = Received mbWalletCredentials

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
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
          }
      for_ mbWallet \wallet -> do
        { mbWalletReloadSec } <- getStore
        mbReloadSchedulerForkId <- case mbWalletReloadSec of
          Just seconds -> do
            forkId <- H.fork $ repeatAction (convertDuration seconds) handleAction Reload
            pure $ Just forkId
          Nothing ->
            pure Nothing
        H.put $ Loaded { wallet, mbReloadSchedulerForkId, isDirtyToggle: false }
        handleAction Reload

    Reload -> do
      state <- H.get
      case state of
        Loaded { isDirtyToggle } -> H.modify_ \state' -> set (_Loaded <<< _dirtyToggle) (not isDirtyToggle) state'
        _ -> pure unit

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action Slots output m (Maybe a)
  handleQuery = case _ of
    ReloadWallet a -> do
      handleAction Reload
      pure $ Just a

  killReloadScheduler :: H.HalogenM State Action Slots output m Unit
  killReloadScheduler = do
    state <- H.get
    case state of
      Loaded { mbReloadSchedulerForkId: Just forkId } -> kill forkId
      _ -> pure unit

  _Loaded :: Prism' State WalletContext
  _Loaded = prism' Loaded case _ of
    Loaded context -> Just context
    _ -> Nothing

  _dirtyToggle :: Lens' WalletContext Boolean
  _dirtyToggle = prop (Proxy :: Proxy "isDirtyToggle")

  render :: State -> H.ComponentHTML Action Slots m
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

  renderContext :: ∀ w i. Context -> HH.HTML w i
  renderContext Nothing =
    HH.text "No wallet connected"

  renderContext (Just wallet) =
    HH.span_
      [ HH.text "Connecting to wallet "
      , HH.text $ show wallet.id
      , HH.text "…"
      ]

  renderWallet :: Wallet -> H.ComponentHTML Action Slots m
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
          , HH.td_ [ HH.slot_ (Proxy :: _ "loadText") 1 (LoadText.component balance) unit ]
          ]
      , HH.tr_
          [ HH.th_ [ HH.text "UTxOs" ]
          , HH.td_ [ HH.slot_ (Proxy :: _ "loadTexts") 1 (LoadTexts.component utxos) unit ]
          ]
      , HH.tr_
          [ HH.th_ [ HH.text "Change Address" ]
          , HH.td_ [ HH.slot_ (Proxy :: _ "loadText") 2 (LoadText.component changeAddress) unit ]
          ]
      , HH.tr_
          [ HH.th_ [ HH.text "Reward Addresses" ]
          , HH.td_ [ HH.slot_ (Proxy :: _ "loadTexts") 2 (LoadTexts.component rewardAddresses) unit ]
          ]
      , HH.tr_
          [ HH.th_ [ HH.text "Used Addresses" ]
          , HH.td_ [ HH.slot_ (Proxy :: _ "loadTexts") 3 (LoadTexts.component usedAddresses) unit ]
          ]
      ]
    where
    balance = map CS.bigNum.toStr <$> getWalletBalance wallet.api
    changeAddress = map formatAddress <$> getWalletChangeAddress wallet.api
    rewardAddresses = bindFlipped fromArray <$> map (map formatAddress) <$> getWalletRewardAddresses wallet.api
    usedAddresses = bindFlipped fromArray <$> map (map formatAddress) <$> getWalletUsedAddresses wallet.api
    utxos = bindFlipped fromArray <$> map (map formatUtxo) <$> getWalletUtxos wallet.api

  formatAddress :: CS.Address -> String
  formatAddress address =
    CS.address.toBech32 address Nothing

  formatUtxo :: CS.TxUnspentOut -> String
  formatUtxo utxo = CS.txHash.toHex (CS.txIn.txId $ CS.txUnspentOut.in utxo)
    <> " #"
    <> show (floor $ CS.txIn.index $ CS.txUnspentOut.in utxo)
    <> " "
    <> CS.bigNum.toStr (CS.value.coin $ CS.txOut.amount $ CS.txUnspentOut.out utxo)


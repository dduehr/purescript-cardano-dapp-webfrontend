module Frontend.Api.Infrastructure.Cip30.WalletApi where

import Prelude

import Cardano.Wallet as CW
import Control.Bind (bindFlipped)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Traversable (traverse)
import Csl (Address, BigNum, Tx, TxUnspentOut, TxWitnessSet, address, bigNum, tx, txUnspentOut, txWitnessSet) as CS
import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff)

import Frontend.Api.Utils (maybeAff)
import Frontend.Capability.LogMessages (class LogMessages)

getWalletBalance :: ∀ m. MonadAff m => LogMessages m => CW.Api -> MaybeT m CS.BigNum
getWalletBalance = MaybeT <<< map (bindFlipped CS.bigNum.fromHex) <<< maybeAff <<< CW.getBalance

getWalletChangeAddress :: ∀ m. MonadAff m => LogMessages m => CW.Api -> MaybeT m CS.Address
getWalletChangeAddress = MaybeT <<< map (bindFlipped CS.address.fromHex) <<< maybeAff <<< CW.getChangeAddress

getWalletNetworkId :: ∀ m. MonadAff m => LogMessages m => CW.Api -> MaybeT m CW.NetworkId
getWalletNetworkId = MaybeT <<< maybeAff <<< CW.getNetworkId

getWalletRewardAddresses :: ∀ m. MonadAff m => LogMessages m => CW.Api -> MaybeT m (Array CS.Address)
getWalletRewardAddresses = MaybeT <<< map (bindFlipped $ traverse CS.address.fromHex) <<< maybeAff <<< CW.getRewardAddresses

getWalletUsedAddresses :: ∀ m. MonadAff m => LogMessages m => CW.Api -> CW.Paginate -> MaybeT m (Array CS.Address)
getWalletUsedAddresses api = MaybeT <<< map (bindFlipped $ traverse CS.address.fromHex) <<< maybeAff <<< CW.getUsedAddresses api

getWalletUtxos :: ∀ m. MonadAff m => LogMessages m => CW.Api -> Maybe CW.Paginate -> MaybeT m (Array CS.TxUnspentOut)
getWalletUtxos api = MaybeT <<< map (bindFlipped $ traverse CS.txUnspentOut.fromHex) <<< maybeAff <<< CW.getUtxos api

signTx :: ∀ m. MonadAff m => LogMessages m => CW.Api -> CS.Tx -> Boolean -> MaybeT m CS.TxWitnessSet
signTx api tx = MaybeT <<< map (bindFlipped CS.txWitnessSet.fromHex) <<< maybeAff <<< CW.signTx api (CS.tx.toHex tx)

submitTx :: ∀ m. MonadAff m => LogMessages m => CW.Api -> CS.Tx -> MaybeT m String
submitTx api tx = MaybeT <<< maybeAff $ CW.submitTx api (CS.tx.toHex tx)
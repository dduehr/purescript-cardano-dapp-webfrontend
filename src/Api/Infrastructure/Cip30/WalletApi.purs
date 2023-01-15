module Frontend.Api.Infrastructure.Cip30.WalletApi where

import Prelude

import Csl (Address, BigNum, Tx, TxUnspentOut, TxWitnessSet) as CS
import Cardano.Wallet (Api, NetworkId, Paginate) as CW
import Control.Monad.Maybe.Trans (MaybeT)
import Data.Maybe (Maybe)

class Monad m <= ManageWalletXpi m where
  getWalletBalance :: CW.Api -> MaybeT m CS.BigNum
  getWalletChangeAddress :: CW.Api -> MaybeT m CS.Address
  getWalletNetworkId :: CW.Api -> MaybeT m CW.NetworkId
  getWalletRewardAddresses :: CW.Api -> MaybeT m (Array CS.Address)
  getWalletUsedAddresses :: CW.Api -> CW.Paginate -> MaybeT m (Array CS.Address)
  getWalletUtxos :: CW.Api -> Maybe CW.Paginate -> MaybeT m (Array CS.TxUnspentOut)
  signTx :: CW.Api -> CS.Tx -> Boolean -> MaybeT m CS.TxWitnessSet
  submitTx :: CW.Api -> CS.Tx -> MaybeT m String
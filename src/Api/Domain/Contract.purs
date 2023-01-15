module Frontend.Api.Domain.Contract where

import Prelude

import Csl (Address, BigNum) as CS
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift) as H

import Frontend.Data.Tx (TxId)
import Frontend.Data.Wallet (WalletApi)

class Monad m <= ManageContract m where
  sendAdaToContract :: WalletApi -> SendAdaToContractFields -> m (Maybe TxId)
  sendTokenToContract :: WalletApi -> SendTokenToContractFields -> m (Maybe TxId)
  redeemAdaFromContract :: WalletApi -> RedeemAdaFromContractFields -> m (Maybe TxId)
  redeemTokenFromContract :: WalletApi -> RedeemTokenFromContractFields -> m (Maybe TxId)

instance manageContractHalogenM ::
  ManageContract m =>
  ManageContract (H.HalogenM state action slots output m) where
  sendAdaToContract api = H.lift <<< sendAdaToContract api
  sendTokenToContract api = H.lift <<< sendTokenToContract api
  redeemAdaFromContract api = H.lift <<< redeemAdaFromContract api
  redeemTokenFromContract api = H.lift <<< redeemTokenFromContract api

type SendAdaToContractFields =
  { contractAddress :: CS.Address
  , lovelaceAmount :: CS.BigNum
  , mbDatum :: Maybe String
  }

type SendTokenToContractFields =
  { contractAddress :: CS.Address
  -- TBD ...
  }

type RedeemAdaFromContractFields =
  { contractAddress :: CS.Address
  -- TBD ...
  }

type RedeemTokenFromContractFields =
  { contractAddress :: CS.Address
  -- TBD ...
  }
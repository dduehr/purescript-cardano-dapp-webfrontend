module Frontend.Capability.Resource.Address where

import Prelude

import Csl (Address, BigNum) as CS
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift) as H

import Frontend.Data.Wallet (WalletApi)
import Frontend.Data.Tx (TxId)

class Monad m <= ManageAddress m where
  sendAdaToAddress :: WalletApi -> SendAdaToAddressFields -> m (Maybe TxId)
  sendTokenToAddress :: WalletApi -> SendTokenToAddressFields -> m (Maybe TxId)

instance manageAddressHalogenM ::
  ManageAddress m =>
  ManageAddress (H.HalogenM state action slots output m) where
  sendAdaToAddress api = H.lift <<< sendAdaToAddress api
  sendTokenToAddress api = H.lift <<< sendTokenToAddress api

type SendAdaToAddressFields =
  { recipientAddress :: CS.Address
  , lovelaceAmount :: CS.BigNum
  }

type SendTokenToAddressFields =
  { recipientAddress :: CS.Address
  -- TBD ...
  }

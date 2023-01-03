module Frontend.Capability.Resource.Address where

import Prelude

import Cardano.Wallet (Api) as CW
import Csl (Address, BigNum) as CS
import Data.Maybe (Maybe)
import Frontend.Data.Tx (TxId)
import Halogen (HalogenM, lift) as H

type SendAdaToAddressFields =
  { recipientAddress :: CS.Address
  , lovelaceAmount   :: CS.BigNum
  }

type SendTokenToAddressFields = 
  { recipientAddress :: CS.Address
  -- TBD ...
  }

class Monad m <= ManageAddress m where
  sendAdaToAddress :: CW.Api -> SendAdaToAddressFields -> m (Maybe TxId)
  sendTokenToAddress :: CW.Api -> SendTokenToAddressFields -> m (Maybe TxId)

instance manageAddressHalogenM :: ManageAddress m => ManageAddress (H.HalogenM state action slots output m) where
  sendAdaToAddress api = H.lift <<< sendAdaToAddress api 
  sendTokenToAddress api = H.lift <<< sendTokenToAddress api 
module Frontend.Capability.Resource.Address where

import Prelude

import Cardano.Wallet (Api) as CW
import Csl (Address, BigNum) as Csl
import Data.Maybe (Maybe)
import Frontend.Data.Transaction (TxId)
import Halogen (HalogenM, lift) as H

type SendAdaToAddressFields =
  { recipientAddress :: Csl.Address
  , lovelaceAmount   :: Csl.BigNum
  }

type SendTokenToAddressFields = 
  { recipientAddress :: Csl.Address
  -- TBD ...
  }

class Monad m <= ManageAddress m where
  sendAdaToAddress :: CW.Api -> SendAdaToAddressFields -> m (Maybe TxId)
  sendTokenToAddress :: CW.Api -> SendTokenToAddressFields -> m (Maybe TxId)

instance manageAddressHalogenM :: ManageAddress m => ManageAddress (H.HalogenM state action slots output m) where
  sendAdaToAddress api = H.lift <<< sendAdaToAddress api 
  sendTokenToAddress api = H.lift <<< sendTokenToAddress api 
module Example.Capability.Resource.Address where

import Prelude

import Cardano.Wallet (Api) as CW
import Control.Monad.Except.Trans (ExceptT)
import Csl (Address, BigNum, TxBuilderConfig) as Csl

type SendAdaToAddressFields =
  { recipientAddress :: Csl.Address
  , lovelaceAmount   :: Csl.BigNum
  }

type SendTokenToAddressFields = 
  { recipientAddress :: Csl.Address
  -- TBD ...
  }

type Error = String
type TxId  = String

class Monad m <= ManageAddress m where
  sendAdaToAddress :: CW.Api -> Csl.TxBuilderConfig -> SendAdaToAddressFields -> ExceptT Error m TxId
  sendTokenToAddress :: CW.Api -> Csl.TxBuilderConfig -> SendTokenToAddressFields -> ExceptT Error m TxId

-- instance manageAddressHalogenM :: ManageAddress m => ManageAddress (H.HalogenM state action slots output m) where
--  sendAdaToAddress wallet = H.lift <<< sendAdaToAddress wallet
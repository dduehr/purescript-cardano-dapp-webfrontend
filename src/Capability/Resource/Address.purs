module Qapla.Capability.Resource.Address where

import Prelude

import Cardano.Wallet (Api) as CW
import Control.Monad.Except.Trans (ExceptT)
import Csl (Address, BigNum) as Csl
import Data.Either (Either)
import Halogen (HalogenM, lift) as H

type SendAdaToAddressFields =
  { recipientAddress :: Csl.Address
  , lovelaceAmount   :: Csl.BigNum
  }

type Error = String
type TxId  = String

class Monad m <= ManageAddress m where
  sendAdaToAddress :: CW.Api -> SendAdaToAddressFields -> ExceptT Error m TxId

-- instance manageAddressHalogenM :: ManageAddress m => ManageAddress (H.HalogenM state action slots output m) where
--  sendAdaToAddress wallet = H.lift <<< sendAdaToAddress wallet
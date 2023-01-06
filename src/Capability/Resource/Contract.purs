module Frontend.Capability.Resource.Contract where

import Prelude

import Cardano.Wallet (Api) as CW
import Csl (Address) as CS
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift) as H

import Frontend.Data.Tx (TxId)

type SendAdaToContractFields =
  { contractAddress :: CS.Address
  -- TBD ...
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

class Monad m <= ManageContract m where
  sendAdaToContract :: CW.Api -> SendAdaToContractFields -> m (Maybe TxId)
  sendTokenToContract :: CW.Api -> SendTokenToContractFields -> m (Maybe TxId)
  redeemAdaFromContract :: CW.Api -> RedeemAdaFromContractFields -> m (Maybe TxId)
  redeemTokenFromContract :: CW.Api -> RedeemTokenFromContractFields -> m (Maybe TxId)

instance manageContractHalogenM :: ManageContract m => ManageContract (H.HalogenM state action slots output m) where
  sendAdaToContract api = H.lift <<< sendAdaToContract api
  sendTokenToContract api = H.lift <<< sendTokenToContract api
  redeemAdaFromContract api = H.lift <<< redeemAdaFromContract api
  redeemTokenFromContract api = H.lift <<< redeemTokenFromContract api
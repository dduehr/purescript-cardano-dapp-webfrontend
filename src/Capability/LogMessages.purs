module Frontend.Capability.LogMessages where

import Prelude

import Halogen (HalogenM, lift) as H

class Monad m <= LogMessages m where
  logMessage :: String -> m Unit

instance logMessagesHalogenM ::
  LogMessages m =>
  LogMessages (H.HalogenM state action slots output m) where
  logMessage = H.lift <<< logMessage

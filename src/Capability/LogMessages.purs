module Frontend.Capability.LogMessages where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (HalogenM, lift) as H

class Monad m <= LogMessages m where
  logMessage :: String -> m Unit

instance logMessagesHalogenM :: LogMessages m => LogMessages (H.HalogenM state action slots output m) where
  logMessage = H.lift <<< logMessage

logHush :: ∀ m message a. LogMessages m => Show message => m (Either message a) -> m (Maybe a)
logHush action =
  action >>= case _ of
    Left message -> logMessage (show message) *> pure Nothing
    Right value -> pure $ Just value

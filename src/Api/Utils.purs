module Frontend.Api.Utils (maybeEffect, maybeAff) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (try)

import Frontend.Capability.LogMessages (class LogMessages, logMessage)

maybeEffect :: ∀ m a. MonadEffect m => LogMessages m => Effect a -> m (Maybe a)
maybeEffect = logHush <<< liftEffect <<< try

maybeAff :: ∀ m a. MonadAff m => LogMessages m => Aff a -> m (Maybe a)
maybeAff = logHush <<< liftAff <<< attempt

logHush :: ∀ m msg a. LogMessages m => Show msg => m (Either msg a) -> m (Maybe a)
logHush action =
  action >>= case _ of
    Left msg -> logMessage (show msg) *> pure Nothing
    Right a -> pure $ Just a
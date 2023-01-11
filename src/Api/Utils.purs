module Frontend.Api.Utils (enumValues, maybeEffect, maybeAff) where

import Prelude

import Data.Bounded.Generic (class GenericBottom, genericBottom)
import Data.Either (Either(..))
import Data.Enum.Generic (class GenericEnum, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (try)

import Frontend.Capability.LogMessages (class LogMessages, logMessage)

enumValues :: ∀ t a rep. Unfoldable t => Generic a rep => GenericEnum rep => GenericBottom rep => t a
enumValues = unfoldr (\maybeValue -> maybeValue >>= next) $ Just genericBottom
  where
  next enumValue = Just $ Tuple enumValue $ genericSucc enumValue

maybeEffect :: ∀ m a. MonadEffect m => LogMessages m => Effect a -> m (Maybe a)
maybeEffect = logHush <<< liftEffect <<< try

maybeAff :: ∀ m a. MonadAff m => LogMessages m => Aff a -> m (Maybe a)
maybeAff = logHush <<< liftAff <<< attempt

logHush :: ∀ m msg a. LogMessages m => Show msg => m (Either msg a) -> m (Maybe a)
logHush action =
  action >>= case _ of
    Left msg -> logMessage (show msg) *> pure Nothing
    Right a -> pure $ Just a
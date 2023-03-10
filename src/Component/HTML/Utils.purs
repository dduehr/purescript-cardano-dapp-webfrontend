module Frontend.Component.HTML.Utils where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Effect.Aff (delay) as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

css :: forall r i. String -> HH.IProp (class :: String | r) i
css = HP.class_ <<< HH.ClassName

maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = HH.text ""

whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""

delayAction
  :: ∀ action state slots output m
   . MonadAff m
  => Milliseconds
  -> (action -> H.HalogenM state action slots output m Unit)
  -> action
  -> H.HalogenM state action slots output m Unit
delayAction ms handleAction action = do
  H.liftAff $ Aff.delay ms
  handleAction action

repeatAction
  :: ∀ action state slots output m
   . MonadAff m
  => Milliseconds
  -> (action -> H.HalogenM state action slots output m Unit)
  -> action
  -> H.HalogenM state action slots output m Unit
repeatAction ms handleAction action = do
  delayAction ms handleAction action
  repeatAction ms handleAction action

spinner :: ∀ w i. HH.HTML w i
spinner =
  HH.span [ css "icon is-small" ]
    [ HH.i [ css "fas fa-solid fa-spinner fa-spin" ] [] ]
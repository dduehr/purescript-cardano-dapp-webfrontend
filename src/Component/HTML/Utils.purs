module Example.Component.HTML.Utils where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

css :: forall r i. String -> HH.IProp (class :: String | r) i
css = HP.class_ <<< HH.ClassName

maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = HH.text ""

whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""

-- FIXME: Move to different module?
exceptAff :: ∀ error a m. MonadAff m => Aff (Either error a) -> ExceptT error m a    
exceptAff = ExceptT <<< liftAff

exceptM :: ∀ error a m. Monad m => m (Either error a) -> ExceptT error m a
exceptM = ExceptT
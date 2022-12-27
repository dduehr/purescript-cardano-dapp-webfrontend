module Example.Store.Connect where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectAll)
import Type.Proxy (Proxy(..))

import Example.Store as Store

type Input = Unit

type Context = Store.Store

deriveState :: Connected Context Input -> Context
deriveState { context } = context

data Action
  = Receive (Connected Store.Store Input)

inputAll
  :: ∀ query output m
   . MonadEffect m
  => MonadStore Store.Action Store.Store m
  => H.Component query Context output m -> H.Component query Input output m
inputAll component =
  connect selectAll $ H.mkComponent
    { initialState: deriveState
    , render: \state -> HH.slot_ (Proxy :: Proxy "inner") unit component state 
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where

    handleAction = case _ of
      Receive connected ->
        H.put $ deriveState connected

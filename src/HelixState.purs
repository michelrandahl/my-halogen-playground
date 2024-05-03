module HelixState where
-- figuring out how to use helix state

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Helix (HelixMiddleware, UseHelix, UseHelixHook, makeStore, makeStore', (|>))
import Halogen.Hooks (ComponentTokens)
import Halogen.Hooks as Hooks

type State = Int

data Action
  = Increment
  | Decrement
  | Initialize Int
derive instance Generic Action _
instance Show Action where
  show = genericShow

reducer :: State -> Action -> State
reducer st = case _ of
  Increment -> st + 1
  Decrement -> st - 1
  Initialize i -> i

type Input = Unit

data Query a
  = QueryInitialize Int a

useCounter :: forall ctx m. MonadAff m => Eq ctx => UseHelixHook State Action ctx m
useCounter = makeStore "counter" reducer 0 middlewareStack

helixCounter :: forall o m. MonadAff m => H.Component Query Input o m
helixCounter = Hooks.component \(tokens :: ComponentTokens Query _ o) input -> Hooks.do
  (Tuple (state :: Int) ctx) <- useCounter identity
  Hooks.useQuery tokens.queryToken \query -> case query of
    QueryInitialize i a -> do
      ctx.dispatch $ Initialize i
      pure $ Just a
  Hooks.pure $ HH.div_ [ HH.text $ "helix counter: " <> show state]

helixCounterAdd :: forall q o m. MonadAff m => H.Component q Input o m
helixCounterAdd =
  Hooks.component \(tokens :: ComponentTokens q _ o) input ->
    Hooks.do
      (Tuple (state :: Int) ctx) <- useCounter identity
      Hooks.pure $
        HH.button
          [HE.onClick $ const $ ctx.dispatch Increment]
          [ HH.text $ "helix-add (current value: " <> show state <> ")" ]

helixCounterSub :: forall q o m. MonadAff m => H.Component q Input o m
helixCounterSub =
  Hooks.component \(tokens :: ComponentTokens q _ o) input ->
    Hooks.do
      (Tuple (state :: Int) ctx) <- useCounter identity
      Hooks.pure $
        HH.button
          [HE.onClick $ const $ ctx.dispatch Decrement]
          [ HH.text $ "helix-sub (current value: " <> show state <> ")"]

actionLogger :: forall m. MonadAff m => HelixMiddleware State Action m
actionLogger ctx action next = do
  log $ "Action dispatched: " <> show action
  next action

stateLogger :: forall m. MonadAff m => HelixMiddleware State Action m
stateLogger ctx action next = do
  ctx.getState >>= show >>> ("Before state: " <> _) >>> log
  next action
  ctx.getState >>= show >>> ("After state: " <> _) >>> log

middlewareStack :: forall m. MonadAff m => HelixMiddleware State Action m
middlewareStack = stateLogger |> actionLogger

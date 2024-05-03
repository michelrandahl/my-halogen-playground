module MyButton where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Output = Clicked
type Input = Int
type State = { val :: Int }
data Action
  = Receive Input
  | Click
data Query a
  = IncButton Int a
  | GetButtonVal (Int -> a)

myButton :: forall m. MonadAff m => H.Component Query Input Output m
myButton =
  H.mkComponent
    { initialState
    , render
    , eval : H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Receive
      }
    }
  where
  initialState :: Input -> State
  initialState input = { val : input}

  render { val } = HH.button [ HE.onClick (const Click)] [ HH.text $ show val ]

  handleAction = case _ of
    Receive input ->
      H.modify_ _ { val = input }
    Click -> do
      log "child button was clicked"
      H.raise Clicked

  handleQuery :: forall action a. Query a -> H.HalogenM State action () Output m (Maybe a)
  handleQuery = case _ of
    IncButton x a -> do
      H.modify_ \state -> state { val = state.val + 1337}
      pure $ Just a

    GetButtonVal reply -> do
      { val } <- H.get
      pure $ Just $ reply val

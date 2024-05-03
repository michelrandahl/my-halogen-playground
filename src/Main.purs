module Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (range)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import HelixState (helixCounter, helixCounterAdd, helixCounterSub, useCounter)
import HelixState as HelixState
import MyButton (myButton)
import MyButton as MyButton
import MyWorker (MyWorker, handleWorkerOutput, mkWorker, sendToWorker)
import MyWorker as MyWorker
import Type.Prelude (Proxy(..))
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI component unit body

  void $ H.liftEffect $ HS.subscribe io.messages \(BToggled newState) -> do
    H.liftEffect $ log $ "Button was internally toggled to: " <> show newState
    pure Nothing
  state0 <- io.query $ H.mkRequest IsOn

  H.liftEffect $ log $ "The button state is currently: " <> show state0
  void $ io.query $ H.mkTell (SetState true)

  state1 <- io.query $ H.mkRequest IsOn
  H.liftEffect $ log $ "The button state is now: " <> show state1

data Query a
  = IsOn (Boolean -> a)
  | SetState Boolean a

data Output = BToggled Boolean

data Action
  = Increment
  | Decrement
  | RandomPlus
  | RandomMinus
  | Initialize
  | Finalize
  | Tick
  | HandleKey H.SubscriptionId KE.KeyboardEvent
  | HandleChildButton MyButton.Output
  | TellChildButton
  | RequestChildButton
  | Toggle
  | HandleWorkerOutput MyWorker.Output
  | HandleHelixStateOutput HelixState.Output

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 1000.0
    H.liftEffect $ HS.notify listener val
  pure emitter

type State =
  { chars   :: String
  , count   :: Int
  , enabled :: Boolean
  , myWorker :: Maybe MyWorker
  }

type Slots =
  ( button :: H.Slot MyButton.Query MyButton.Output Unit
  , helixCounter  :: H.Slot HelixState.Query HelixState.Output Unit
  , helixCounterAdd :: forall q o. H.Slot q o Unit
  , helixCounterSub :: forall q o. H.Slot q o Unit
  )

_button = Proxy :: Proxy "button"
_helixCounter = Proxy :: Proxy "helixCounter"
_helixCounterAdd = Proxy :: Proxy "helixCounterAdd"
_helixCounterSub = Proxy :: Proxy "helixCounterSub"

component :: forall input m. MonadAff m => H.Component Query input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery  = handleQuery
      , initialize   = Just Initialize
      , finalize     = Just Finalize
      }
    }
  where
  initialState :: input -> State
  initialState _ =
    { count : 0
    , chars : ""
    , enabled : false
    , myWorker : Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render {count,chars,enabled} =
    HH.div_
      [ element
      , HH.div_ []
      , HH.button [HE.onClick (const Toggle)] [ HH.text $ "special button: " <> show enabled ]
      -- underscore suffix can in some cases be used when we don't have any attributes
      , HH.div_ [ HH.text chars ]
      , HH.button [ HE.onClick (const Decrement) ] [ HH.text "-" ]
      , HH.button [ HE.onClick (const RandomMinus) ] [ HH.text "random-minus" ]
      , HH.div_ [ HH.text $ show count ]
      , HH.button [ HE.onClick (const Increment) ] [ HH.text "+" ]
      , HH.button [ HE.onClick (const RandomPlus) ] [ HH.text "random-plus (using web-worker)" ]
      , htmlExample
      , HH.div_ [ HH.slot _button unit myButton count  HandleChildButton ]
      , HH.button [ HE.onClick (const TellChildButton) ] [ HH.text "tell child button" ]
      , HH.button [ HE.onClick (const RequestChildButton) ] [ HH.text "request child button" ]
      , HH.div_ [ ]
      , mySelect
      , HH.slot _helixCounter unit helixCounter unit HandleHelixStateOutput
      , HH.slot_ _helixCounterAdd unit helixCounterAdd unit
      , HH.slot_ _helixCounterSub unit helixCounterSub unit
      ]

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Output m (Maybe a)
  handleQuery = case _ of
    IsOn reply -> do
      log "IsOn"
      enabled <- H.gets _.enabled
      pure $ Just $ reply enabled

    SetState enabled a -> do
      log "SetState"
      H.modify_ (_ {enabled = enabled})
      pure $ Just a

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of
    Initialize -> do
      H.tell _helixCounter unit $ HelixState.QueryInitialize 42

      myWorker <- H.liftEffect mkWorker
      { emitter, listener } <- H.liftEffect HS.create
      H.liftEffect $ handleWorkerOutput myWorker $
        case _ of
          Just output -> do
            log $ "got output from worker " <> show output
            HS.notify listener $ HandleWorkerOutput output
          _ -> log $ "invalid output from worker"
      _ <- H.subscribe emitter
      -- testing that the worker can receive input
      H.liftEffect $
        sendToWorker myWorker $ MyWorker.Hello $ MyWorker.Msg "msg from main"
      H.modify_ _ {myWorker = Just myWorker}

      _ <- H.subscribe =<< timer Tick
      handleAction RandomPlus
      document <- H.liftEffect $ document =<< window
      H.subscribe' \sid ->
        eventListener
          KET.keyup
          (HTMLDocument.toEventTarget document)
          (map (HandleKey sid) <<< KE.fromEvent)

    Increment -> H.modify_ \state -> state {count = state.count + 1}

    Decrement -> H.modify_ \state -> state {count = state.count - 1}

    RandomMinus -> do
      newNumber <- H.liftEffect $ randomInt 1 100
      H.modify_ \state -> state {count = state.count - newNumber}

    RandomPlus  -> do
      state <- H.get
      newNumber <- H.liftEffect $ randomInt 1 100
      for_ state.myWorker \w -> do
        H.liftEffect $
          sendToWorker w $ MyWorker.AddNumbers $ Tuple newNumber state.count

    Finalize -> do
      count <- H.gets \s -> s.count
      log $ "finalized, last count: " <> show count

    Tick -> handleAction Increment

    HandleKey sid ev
      | KE.shiftKey ev -> do
        H.liftEffect $ E.preventDefault $ KE.toEvent ev
        let char = KE.key ev
        when (String.length char == 1) do
          H.modify_ \st -> st {chars = st.chars <> char}
      | KE.key ev == "Enter" -> do
        H.liftEffect $ E.preventDefault $ KE.toEvent ev
        H.modify_ _ { chars = "" }
        H.unsubscribe sid
      | otherwise -> pure unit

    HandleChildButton MyButton.Clicked -> do
      log "child button click registered at parent"
      H.modify_ (\state -> state { count = state.count + 1000 })
    
    TellChildButton -> do
      H.tell _button unit (MyButton.IncButton 42)

    RequestChildButton -> do
      childButtonVal <- H.request _button unit MyButton.GetButtonVal
      case childButtonVal of
        Just v -> H.modify_ (\state -> state { count = v })
        Nothing -> pure unit

    Toggle -> do
      newState <- H.modify \st -> st { enabled = not st.enabled }
      H.raise $ BToggled newState.enabled

    HandleWorkerOutput (MyWorker.AddRes res) -> do
      log $ "got res from worker " <> show res
      H.modify_ \state -> state {count = state.count + res}
    HandleWorkerOutput (MyWorker.MultRes res) -> pure unit

    HandleHelixStateOutput o -> do
      log $ "got result from HelixState component: " <> show o

element :: forall w2 i3. HH.HTML w2 i3
element = HH.h1 [ ] [ HH.text "Hello, world!" ]

mySelect :: forall w2 i3. HH.HTML w2 i3
mySelect =
  HH.select
    [ HP.multiple true ]
    (range 1 3 <#> mkOption)
  where
    mkOption :: Int -> _
    mkOption n = HH.option [ HP.value $ "stuff" <> (show n) ] [ HH.text $ "stuff" <> show n ]

htmlExample :: forall w2 i3. HH.HTML w2 i3
htmlExample =
  HH.div
    [ HP.id "root" ]
    [ HH.input
        [ HP.placeholder "Name" ]
    , HH.button
        [ HP.classes [ HH.ClassName "btn-primary" ]
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Submit" ]
    ]

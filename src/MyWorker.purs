module MyWorker where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (Nullable, notNull, null, toMaybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import Web.Worker.DedicatedWorkerGlobalScope (onMessage, postMessage)
import Web.Worker.MessageEvent (data_)
import Web.Worker.Options (defaultWorkerOptions)
import Web.Worker.Types (class IsSendable)
import Web.Worker.Worker (Worker, terminate)
import Web.Worker.Worker as Worker


-- we can communicate newtypes between host app and web-worker
newtype Msg = Msg String
derive instance Newtype Msg _

-- when communicating from host app to web-worker we have to do it with a js-friendly datatype
-- .... using normal sum-types is not directly possible,
-- so we create a record type with nullable field for each case
type RawInput =
  -- newtypes goes well 'over-the-wire'
  { hello :: Nullable Msg
  -- Tuple goes well 'over-the-wire'
  , addNumbers :: Nullable (Tuple Int Int)
  -- Array goes well 'over-the-wire'
  , multNumbers :: Nullable (Array Int)
  }

emptyRawInput :: RawInput
emptyRawInput =
  { hello : null
  , addNumbers : null
  , multNumbers : null
  }

data Input
  = Hello Msg
  | AddNumbers (Tuple Int Int)
  | MultNumbers (Array Int)

inputFromRaw :: RawInput -> Maybe Input
inputFromRaw raw
    = Hello <$> toMaybe raw.hello
  <|> AddNumbers <$> toMaybe raw.addNumbers
  <|> MultNumbers <$> toMaybe raw.multNumbers

inputToRaw :: Input -> RawInput
inputToRaw (Hello msg) = emptyRawInput {hello = notNull msg}
inputToRaw (AddNumbers numbers) = emptyRawInput {addNumbers = notNull numbers}
inputToRaw (MultNumbers numbers) = emptyRawInput {multNumbers = notNull numbers}

newtype InputForeign = InputForeign Foreign
derive instance Newtype InputForeign _
instance IsSendable InputForeign

toInputForeign :: Input -> InputForeign
toInputForeign =
  inputToRaw
  >>> unsafeToForeign
  >>> wrap

fromInputForeign :: Foreign -> Maybe Input
fromInputForeign =
  (unsafeFromForeign :: _ -> InputForeign)
  >>> unwrap
  >>> (unsafeFromForeign :: _ -> RawInput)
  >>> inputFromRaw

-- same qirks applies to outputs from the web-worker when
-- they need to be send from web-worker to host application
data Output
  = AddRes Int
  | MultRes Int
derive instance Generic Output _
instance Show Output where
  show = genericShow

type RawOutput =
  { addRes :: Nullable Int
  , multRes :: Nullable Int
  }

emptyRawOutput :: RawOutput
emptyRawOutput = {addRes : null, multRes : null}

outputFromRaw :: RawOutput -> Maybe Output
outputFromRaw raw
    = AddRes <$> toMaybe raw.addRes
  <|> MultRes <$> toMaybe raw.multRes

outputToRaw :: Output -> RawOutput
outputToRaw (AddRes x) = emptyRawOutput {addRes = notNull x}
outputToRaw (MultRes x) = emptyRawOutput {multRes = notNull x}

newtype OutputForeign = OutputForeign Foreign
derive instance Newtype OutputForeign _
instance IsSendable OutputForeign

toOutputForeign :: Output -> OutputForeign
toOutputForeign =
  outputToRaw
  >>> unsafeToForeign
  >>> wrap

fromOutputForeign :: Foreign -> Maybe Output
fromOutputForeign =
  (unsafeFromForeign :: _ -> OutputForeign)
  >>> unwrap
  >>> (unsafeFromForeign :: _ -> RawOutput)
  >>> outputFromRaw

newtype MyWorker = MyWorker Worker
derive instance Newtype MyWorker _

handleWorkerOutput :: MyWorker -> (Maybe Output -> Effect Unit) -> Effect Unit
handleWorkerOutput (MyWorker w) f =
  w # Worker.onMessage (data_ >>> fromOutputForeign >>> f)

sendToWorker :: MyWorker -> Input -> Effect Unit
sendToWorker (MyWorker worker) input =
  worker # Worker.postMessage (toInputForeign input)

postFromWorker :: Output -> Effect Unit
postFromWorker = postMessage <<< toOutputForeign

mkWorker :: Effect MyWorker
mkWorker =
  -- note that we here refer to the bundled file created in `mk-workers.sh`
  Worker.new "./my-worker.js" defaultWorkerOptions
  <#> wrap

terminateWorker :: MyWorker -> Effect Unit
terminateWorker = terminate <<< unwrap

main :: Effect Unit
main =
  launchAff_ do
    liftEffect $ onMessage \ev -> do
      let (input :: Maybe Input) = fromInputForeign $ data_ ev
      for_ input $
        case _ of
          Hello (Msg msg) -> log $ "Hello " <> msg
          AddNumbers (Tuple x y) -> do
            log "adding numbers"
            let result = x + y
            AddRes result
              # postFromWorker
          MultNumbers numbers -> do
            log "mult numbers"
            let result = Array.foldl (*) 1 numbers
            MultRes result
              # postFromWorker

module Main exposing (..)

import Json.Decode as Decode
import Task
import Time exposing (Time)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import BinaryDecoder.File as File exposing (File)
import BinaryDecoder.Byte as Byte exposing (ArrayBuffer, Error)
import SmfDecoder exposing (Smf)
import ErrorFormatter
import Midi exposing (Midi)
import MidiPlayer


main : Program Never Model Msg
main =
  program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { midi : Maybe Midi
  , playing : Bool
  , startTime : Time
  , currentTime : Time
  , error : Error
  }


type Error
  = NoError
  | DecodeError ArrayBuffer Byte.Error


get : (data -> Maybe a) -> data -> a
get f data =
  case f data of
    Just a ->
      a

    Nothing ->
      Debug.crash "undefined"


see : (data -> Maybe a) -> data -> b -> b
see f data b =
  case f data of
    Just a ->
      b

    Nothing ->
      Debug.crash "undefined"


type Msg
  = GotFile File
  | ReadBuffer (Result File.Error ArrayBuffer)
  | Back
  | Start Time
  | Stop
  | Tick Time
  | Timed (Time -> Msg)


init : (Model, Cmd Msg)
init =
  (Model Nothing False 0 0 NoError, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotFile file ->
      ( model
      , Task.attempt ReadBuffer (File.readFileAsArrayBuffer file)
      )

    ReadBuffer (Ok buf) ->
      case Byte.decode SmfDecoder.smf buf of
        Ok smf ->
          ({ model
             | midi = Just (Midi.fromSmf smf)
          }, Cmd.none)

        Err e ->
          ({ model
             | error = DecodeError buf e
          }, Cmd.none)

    ReadBuffer (Err e) ->
      Debug.crash "failed to read arrayBuffer"

    Back ->
      ({ model
          | startTime = 0
          , currentTime = 0
          , playing = False
      }, Cmd.none )

    Start currentTime ->
      ({ model
          | startTime = if model.currentTime > 0 then currentTime - (model.currentTime - model.startTime) else currentTime
          , currentTime = currentTime
          , playing = True
      }, Cmd.none )

    Stop ->
      ({ model
          | playing = False
      }, Cmd.none )

    Tick currentTime ->
      ({ model
          | currentTime = currentTime
      }, Cmd.none )

    Timed toMsg ->
      ( model, Task.perform toMsg Time.now )


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.playing then
    Time.every (1000 * Time.millisecond / 30) Tick
  else
    Sub.none


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "MIDI Player" ]
    , fileLoadButton "audio/mid" GotFile
    , case model.midi of
        Just midi ->
          MidiPlayer.view
            { onBack = Back
            , onStart = Timed Start
            , onStop = Stop
            }
            model.playing
            (model.currentTime - model.startTime)
            midi

        _ ->
          text ""
    , case model.error of
        NoError ->
          text ""

        DecodeError buf e ->
          ErrorFormatter.print buf e
    ]


fileLoadButton : String -> (File -> msg) -> Html msg
fileLoadButton accept_ tagger =
  input
    [ type_ "file"
    , accept accept_
    , on "change" (File.targetFile tagger)
    ]
    [ text "load" ]

port module Main exposing (..)

import Array
import Json.Decode as Decode
import Task
import Time exposing (Time)
import Process
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import BinaryDecoder.File as File exposing (File)
import BinaryDecoder.Byte as Byte exposing (ArrayBuffer, Error)
import SmfDecoder exposing (Smf)
import ErrorFormatter
import Midi exposing (Midi, Note, Detailed)
import MidiPlayer
import WebMidiApi exposing (MidiPort, MidiAccess, MidiOutEvent, MidiInEvent)


port start : () -> Cmd msg
port stop : () -> Cmd msg


main : Program Never Model Msg
main =
  program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = lazy view
    }


type alias Model =
  { midi : Maybe Midi
  , playing : Bool
  , startTime : Time
  , currentTime : Time
  , futureNotes : List (Detailed Note)
  , midiIns : List MidiPort
  , midiOuts : List MidiPort
  , selectedMidiOut : Maybe String
  , showConfig : Bool
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
  | ReceiveMidiAccess MidiAccess
  | SelectMidiOut Int String
  | Send Int WebMidiApi.MidiMessage
  | ToggleTrack Int
  | ToggleConfig
  | ReceiveMidiInEvent MidiInEvent


init : (Model, Cmd Msg)
init =
  ( Model Nothing False 0 0 [] [] [] Nothing False NoError
  , WebMidiApi.requestMidiAccess ()
  )


andThen : (model -> (model, Cmd msg)) -> (model, Cmd msg) -> (model, Cmd msg)
andThen f (model, cmd) =
  let
    (newModel, newCmd) =
      f model
  in
    newModel ! [ cmd, newCmd ]


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
          ( { model
              | midi = Just (Midi.fromSmf smf)
            }
          , WebMidiApi.requestMidiAccess ()
          )

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
      ( { model
          | startTime =
              if model.currentTime > 0 then
                currentTime - (model.currentTime - model.startTime)
              else
                currentTime
          , currentTime = currentTime
          , playing = True
          , futureNotes = prepareFutureNotes (get .midi model)
        }
      , start ()
      )
        |> andThen (update (Tick currentTime))

    Stop ->
      ( { model
          | playing = False
        }
      , stop ()
      )

    Tick currentTime ->
      let
        (futureNotes, cmd) =
          sendNotes
            (get .midi model).timeBase
            model.startTime
            currentTime
            model.futureNotes
      in
        ({ model
            | currentTime = currentTime
            , futureNotes = futureNotes
        }, cmd )

    Timed toMsg ->
      ( model, Task.perform toMsg Time.now )

    ReceiveMidiAccess midiAccess ->
      ( { model
          | midiIns = midiAccess.inputs
          , midiOuts = midiAccess.outputs
          , midi =
              model.midi
                |> Maybe.map (\midi ->
                    List.head midiAccess.outputs
                      |> Maybe.map (\midiOut -> Midi.setMidiOutToAllTracks midiOut.id midi)
                      |> Maybe.withDefault midi
                  )
        }
      , Cmd.none
      )

    SelectMidiOut index portId ->
      ( { model | midi = Just (Midi.setMidiOut index portId <| get .midi model) }
      , Cmd.none
      )

    Send trackIndex message ->
      ( model
      , if model.playing then
          model.midi
            |> Maybe.map .tracks
            |> Maybe.andThen (List.drop trackIndex >> List.head)
            |> Maybe.andThen .portId
            |> Maybe.map (\portId -> WebMidiApi.send (MidiOutEvent portId message 0))
            |> Maybe.withDefault Cmd.none
        else
          Cmd.none
      )

    ToggleTrack index ->
      ( { model | midi = Just (Midi.toggleVisibility index <| get .midi model) }
      , Cmd.none
      )

    ToggleConfig ->
      ( { model | showConfig = not model.showConfig }
      , Cmd.none
      )

    ReceiveMidiInEvent midiInEvent ->
      (model, Cmd.none)


prepareFutureNotes : Midi -> List (Detailed Note)
prepareFutureNotes midi =
  midi.tracks
    |> List.indexedMap (,)
    |> List.concatMap (\(index, track) -> List.map (Midi.addDetails index track.channel) track.notes )
    |> List.sortBy .position


sendNotes : Int -> Time -> Time -> List (Detailed Note) -> (List (Detailed Note), Cmd Msg)
sendNotes timeBase startTime currentTime futureNotes =
  let
    time =
      currentTime - startTime

    (newNotes, newFutureNotes) =
      splitWhile
        (\note -> Midi.positionToTime timeBase note.position < time + 1000.0)
        []
        futureNotes

    cmd =
      newNotes
        |> List.map (\note -> (Basics.max 0 (Midi.positionToTime timeBase note.position - time), note))
        |> List.concatMap (\(after, note) ->
            [ Process.sleep after |> Task.perform (\_ -> Send note.track [ 0x90 + note.channel, note.note, note.velocity ])
            , Process.sleep (after + Midi.positionToTime timeBase note.length) |> Task.perform (\_ -> Send note.track [ 0x80 + note.channel, note.note, 0 ])
            ]
          )
        |> Cmd.batch
  in
    (newFutureNotes, cmd)


splitWhile : (a -> Bool) -> List a -> List a -> (List a, List a)
splitWhile f taken list =
  case list of
    [] ->
      (taken, [])

    x :: xs ->
      if f x then
        splitWhile f (x :: taken) xs
      else
        (taken, list)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ WebMidiApi.receiveMidiAccess ReceiveMidiAccess
    , WebMidiApi.receive ReceiveMidiInEvent
    , if model.playing then
        Time.every (1000 * Time.millisecond / 30) Tick
      else
        Sub.none
    ]


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
            , onToggleTrack = ToggleTrack
            , onToggleConfig = ToggleConfig
            , onSelectMidiOut = SelectMidiOut
            }
            model.showConfig
            model.midiOuts
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

module Main exposing (..)

import Json.Decode as Decode
import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import BinaryDecoder.File as File exposing (File)
import BinaryDecoder.Byte as Byte exposing (ArrayBuffer, Error)
import SmfDecoder exposing (Smf)
import ErrorFormatter
import MidiRenderer


main : Program Never Model Msg
main =
  program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { smf : Maybe (Result (Error, ArrayBuffer) Smf)
  }

type Msg
  = GotFile File
  | ReadBuffer (Result File.Error ArrayBuffer)


init : (Model, Cmd Msg)
init =
  (Model Nothing, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotFile file ->
      ( model
      , Task.attempt ReadBuffer (File.readFileAsArrayBuffer file)
      )

    ReadBuffer (Ok buf) ->
      ({ model |
          smf =
            Byte.decode SmfDecoder.smf buf
              |> Result.mapError (\e -> (e, buf))
              |> Just
      }, Cmd.none)

    ReadBuffer (Err e) ->
      Debug.crash "failed to read arrayBuffer"


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "MIDI Decoder" ]
    , fileLoadButton "audio/mid" GotFile
    , case model.smf of
        Just (Ok smf) -> MidiRenderer.renderSmf smf
        Just (Err (e, buf)) -> ErrorFormatter.print buf e
        _ -> text ""
    ]


fileLoadButton : String -> (File -> msg) -> Html msg
fileLoadButton accept_ tagger =
  input
    [ type_ "file"
    , accept accept_
    , on "change" (File.targetFile tagger)
    ]
    [ text "load" ]

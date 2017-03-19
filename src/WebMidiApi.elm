port module WebMidiApi exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode


type alias MidiMessage =
  List Int


type alias MidiOutMessage =
  { portId : String
  , message : MidiMessage
  }


port requestMidiOuts : () -> Cmd msg
port send : MidiOutMessage -> Cmd msg


type alias MidiOut =
  { id : String
  , name : String
  }


port receiveMidiOuts : (List MidiOut -> msg) -> Sub msg


viewSelect : (String -> msg) -> List MidiOut -> Maybe String -> Html msg
viewSelect toMsg midiOuts selectedMidiOut =
  select
    [ onChange toMsg ]
    ( List.map (viewOption selectedMidiOut) midiOuts )


viewOption : Maybe String -> MidiOut -> Html msg
viewOption selectedMidiOut midiOut =
  option [ value midiOut.id ] [ text midiOut.name ]


onChange : (String -> msg) -> Html.Attribute msg
onChange toMsg =
  on "change" (Decode.map toMsg targetValue)

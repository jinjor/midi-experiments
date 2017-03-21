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


port requestMidiAccess : () -> Cmd msg
port send : MidiOutMessage -> Cmd msg


type alias MidiPort =
  { id : String
  , name : String
  }


type alias MidiAccess =
  { inputs : List MidiPort
  , outputs : List MidiPort
  }


port receiveMidiAccess : (MidiAccess -> msg) -> Sub msg


viewSelect : (String -> msg) -> List MidiPort -> Maybe String -> Html msg
viewSelect toMsg midiPorts selectedMidiPort =
  select
    [ onChange toMsg ]
    ( List.map (viewOption selectedMidiPort) midiPorts )


viewOption : Maybe String -> MidiPort -> Html msg
viewOption selectedMidiPort midiPorts =
  option [ value midiPorts.id ] [ text midiPorts.name ]


onChange : (String -> msg) -> Html.Attribute msg
onChange toMsg =
  on "change" (Decode.map toMsg targetValue)

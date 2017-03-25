port module WebMidiApi exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Time exposing (Time)


type alias MidiMessage =
  List Int


type alias MidiOutEvent =
  { portId : String
  , message : MidiMessage
  , at : Time
  }


type alias MidiInEvent =
  { portId : String
  , message : MidiMessage
  }


type alias MidiPort =
  { id : String
  , name : String
  }


type alias MidiAccess =
  { inputs : List MidiPort
  , outputs : List MidiPort
  }


port requestMidiAccess : () -> Cmd msg
port receiveMidiAccess : (MidiAccess -> msg) -> Sub msg
port send : List MidiOutEvent -> Cmd msg
port receive : (MidiInEvent -> msg) -> Sub msg


viewSelect : (String -> msg) -> List MidiPort -> Maybe String -> Html msg
viewSelect toMsg midiPorts selectedMidiPort =
  select
    [ onChange toMsg, style selectStyles ]
    ( List.map (viewOption selectedMidiPort) midiPorts )


(=>) = (,)


selectStyles : List (String, String)
selectStyles =
  [ "-webkit-background-clip" => "content-box"
  , "height" => "18px"
  , "position" => "relative"
  , "top" => "6px"
  , "margin-left" => "7px"
  ]


viewOption : Maybe String -> MidiPort -> Html msg
viewOption selectedMidiPort midiPorts =
  option [ value midiPorts.id, selected (selectedMidiPort == Just midiPorts.id) ] [ text midiPorts.name ]


onChange : (String -> msg) -> Html.Attribute msg
onChange toMsg =
  on "change" (Decode.map toMsg targetValue)

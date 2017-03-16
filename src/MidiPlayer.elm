module MidiPlayer exposing (Options, view)

import Time exposing (Time)
import Dict exposing (Dict)
import Html as H exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Keyed
import Midi exposing (..)


type alias Options msg =
  { onStart : msg
  , onStop : msg
  }


view : Options msg -> Bool -> Time -> Midi -> Html msg
view options playing time midi =
  div []
    [ control options playing
    , midi.tracks
        |> List.map (viewTrack time)
        |> svg containerStyles
    ]


control : Options msg -> Bool -> Html msg
control options playing =
  div [] [ playButton options playing ]


containerStyles : List (S.Attribute msg)
containerStyles =
  [ SA.width "40000"
  , SA.height "300px"
  , viewBox (String.join " " <| List.map toString [0, 0, 10000, 60])
  , preserveAspectRatio "none"
  , HA.style
      [ ("width", "800px")
      , ("height", "300px")
      , ("background-color", "black")
      ]
  ]


playButton : Options msg -> Bool -> Html msg
playButton options playing =
  button
    [ onClick (if playing then options.onStop else options.onStart ) ]
    [ H.text (if playing then "Stop" else "Start" ) ]


viewTrack : Time -> Track -> Html msg
viewTrack time track =
  track.notes
    |> List.map (\note -> (Midi.toKey note, viewNote note))
    |> Svg.Keyed.node "g" [ ]


viewNote : Note -> Html msg
viewNote note =
  rect
    [ x (toString note.position)
    , y (toString <| 60 - (note.note - 30))
    , SA.width (toString note.length)
    , SA.height "1"
    , fill "pink"
    ]
    []


px : a -> String
px num =
  toString num ++ "px"

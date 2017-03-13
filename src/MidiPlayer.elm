module MidiPlayer exposing (Options, view)

import Time exposing (Time)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (..)
import Midi exposing (..)


type alias Options msg =
  { onStart : msg
  , onStop : msg
  }


view : Options msg -> Bool -> Time -> Midi -> Html msg
view options playing time midi =
  div []
    [ playButton options playing
    , midi.tracks
        |> List.map (viewTrack time)
        |> div []
    ]


playButton : Options msg -> Bool -> Html msg
playButton options playing =
  button
    [ onClick (if playing then options.onStop else options.onStart ) ]
    [ text (if playing then "Stop" else "Start" ) ]


viewTrack : Time -> Track -> Html msg
viewTrack time track =
  track.notes
    |> List.map (\note -> (Midi.toKey note, viewNote note))
    |> Keyed.node "div" [ style trackStyle ]


trackStyle : List (String, String)
trackStyle =
  [ ("position", "relative")
  , ("background-color", "black")
  , ("height", "60px")
  , ("margin-bottom", "2px")
  ]


viewNote : Note -> Html msg
viewNote note =
  div [ style (noteStyle note) ] []


noteStyle : Note -> List (String, String)
noteStyle note =
  [ ("position", "absolute")
  , ("left", px <| note.position // 100)
  , ("top", px <| 60 - (note.note - 30) )
  , ("height", "1px")
  , ("width", px <| note.length // 100)
  , ("background-color", "pink")
  ]


px : a -> String
px num =
  toString num ++ "px"

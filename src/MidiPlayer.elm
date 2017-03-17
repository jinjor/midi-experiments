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
  let
    currentPosition =
      floor <| (toFloat midi.timeBase * 2.0 / 1000) * time
  in
    div [ HA.style [ ("position", "relative") ] ]
      [ midi.tracks
          |> List.map (viewTrack currentPosition)
          |> svg (containerStyles currentPosition)
      , control options playing
      ]


containerStyles : Int -> List (S.Attribute msg)
containerStyles currentPosition =
  [ SA.width "10000"
  , SA.height "90"
  , viewBox (String.join " " <| List.map toString [currentPosition, 0, 10000, 90])
  , preserveAspectRatio "none"
  , HA.style
      [ ("width", "480px")
      , ("height", "270px")
      , ("background-color", "black")
      , ("display", "block")
      ]
  ]


control : Options msg -> Bool -> Html msg
control options playing =
  div [ HA.style controlStyles ] [ playButton options playing ]


controlStyles : List (String, String)
controlStyles =
  [ ("width", "480px")
  , ("height", "30px")
  , ("background-color", "rgba(255, 255, 255, 0.1)")
  , ("color", "#eee")
  , ("position", "absolute")
  , ("bottom", "0")
  ]


playButton : Options msg -> Bool -> Html msg
playButton options playing =
  div
    [ onClick (if playing then options.onStop else options.onStart )
    , HA.style buttonStyles
    ]
    [ svg
      [ SA.width "40", SA.height "30" ]
      [ S.path [ SA.fill "#ddd", if playing then stop else start ] [] ]
    ]

start : S.Attribute msg
start =
  SA.d "M10,8v14l16,-7z"


stop : S.Attribute msg
stop =
  SA.d "M10,8v14h4v-14zM20,8v14h4v-14z"


buttonStyles : List (String, String)
buttonStyles =
  [ ("width", "40px")
  , ("bottom", "0")
  , ("text-align", "center")
  ]



viewTrack : Int -> Track -> Html msg
viewTrack currentPosition track =
  track.notes
    |> List.filterMap (\note ->
      if currentPosition < note.position + note.length && currentPosition > note.position - 10000 then
        Just (Midi.toKey note, lazy2 viewNote False note)
      else
        Nothing
      )
    |> Svg.Keyed.node "g" []


viewNote : Bool -> Note -> Html msg
viewNote heighlight note =
  rect
    [ x (toString <| note.position)
    , y (toString <| 90 - (note.note - 60 + 45))
    , SA.width (toString note.length)
    , SA.height "1"
    , fill "pink"
    ]
    []


px : a -> String
px num =
  toString num ++ "px"

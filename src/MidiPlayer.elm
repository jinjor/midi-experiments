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
import Colors


type alias Options msg =
  { onBack : msg
  , onStart : msg
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
          |> List.map2 (viewTrack currentPosition) (Colors.depth 3)
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
  div
    [ HA.style controlStyles ]
    [ backButton options
    , playButton options playing
    ]


controlStyles : List (String, String)
controlStyles =
  [ ("width", "480px")
  , ("height", "30px")
  , ("background-color", "rgba(255, 255, 255, 0.1)")
  , ("color", "#eee")
  , ("position", "absolute")
  , ("bottom", "0")
  , ("display", "flex")
  ]


backButton : Options msg -> Html msg
backButton options =
  controlButton
    ( onClick options.onBack )
    ( S.path [ SA.fill "#ddd", back ] [] )


playButton : Options msg -> Bool -> Html msg
playButton options playing =
  controlButton
    ( onClick (if playing then options.onStop else options.onStart ) )
    ( S.path [ SA.fill "#ddd", if playing then stop else start ] [] )


controlButton : H.Attribute msg -> Svg msg -> Html msg
controlButton event inner =
  div
    [ event, HA.style buttonStyles ]
    [ svg [ SA.width "40", SA.height "30" ] [ inner ] ]


back : S.Attribute msg
back =
  SA.d "M12,10v10h2v-10zm14,0v10l-12,-5z"


start : S.Attribute msg
start =
  SA.d "M10,8v14l16,-7z"


stop : S.Attribute msg
stop =
  SA.d "M10,8v14h4v-14zm10,0v14h4v-14z"


buttonStyles : List (String, String)
buttonStyles =
  [ ("width", "40px")
  , ("bottom", "0")
  , ("text-align", "center")
  ]


viewTrack : Int -> String -> Track -> Html msg
viewTrack currentPosition color track =
  track.notes
    |> List.filterMap (\note ->
      if currentPosition < note.position + note.length && currentPosition > note.position - 10000 then
        Just (Midi.toKey note, lazy2 viewNote color note)
      else
        Nothing
      )
    |> Svg.Keyed.node "g" []


viewNote : String -> Note -> Html msg
viewNote color note =
  rect
    [ x (toString <| note.position)
    , y (toString <| 90 - (note.note - 60 + 45))
    , SA.width (toString note.length)
    , SA.height "1"
    , fill color
    ]
    []


px : a -> String
px num =
  toString num ++ "px"

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
  , onToggleTrack : Int -> msg
  }


type alias NoteColor =
  { highlight : String
  , normal : String
  }


colors : List NoteColor
colors =
  List.map2 NoteColor (Colors.depth 1) (Colors.depth 5)


view : Options msg -> Bool -> Time -> Midi -> Html msg
view options playing time midi =
  let
    currentPosition =
      Midi.timeToPosition midi.timeBase time
  in
    div [ HA.style [ ("position", "relative") ] ]
      [ midi.tracks
          |> List.map2 (viewTrack currentPosition) colors
          |> svg (containerStyles currentPosition)
      , centerLine
      , control options midi.tracks playing
      ]


centerLine : Html msg
centerLine =
  div
    [ HA.style
        [ ("border-right", "solid 1px #555")
        , ("height", "270px")
        , ("left", "240px")
        , ("top", "0")
        , ("position", "absolute")
        ]
    ]
    []


containerStyles : Int -> List (S.Attribute msg)
containerStyles currentPosition =
  [ SA.width "10000"
  , SA.height "90"
  , viewBox (String.join " " <| List.map toString [currentPosition - 5000, 0, 10000, 90])
  , preserveAspectRatio "none"
  , HA.style
      [ ("width", "480px")
      , ("height", "270px")
      , ("background-color", "black")
      , ("display", "block")
      ]
  ]


control : Options msg -> List Track -> Bool -> Html msg
control options tracks playing =
  div
    [ HA.style controlStyles ]
    [ backButton options
    , playButton options playing
    , trackButtons options tracks
    ]


controlStyles : List (String, String)
controlStyles =
  [ ("width", "480px")
  , ("height", "30px")
  , ("background-color", "rgba(255,55,25,0.18)")
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
    ( onClick ( if playing then options.onStop else options.onStart ) )
    ( S.path [ SA.fill "#ddd", if playing then stop else start ] [] )


trackButtons : Options msg -> List Track -> Html msg
trackButtons options tracks =
  tracks
    |> List.map2 (,) colors
    |> List.indexedMap (trackButton options)
    |> div [ HA.style [ ("display", "flex"), ("margin-left", "auto"), ("padding-right", "10px") ] ]


trackButton : Options msg -> Int -> (NoteColor, Track) -> Html msg
trackButton options index (color, track) =
  div
    [ onClick (options.onToggleTrack index)
    , HA.style (trackButtonStyles track.isVisible)
    ]
    [ div [ HA.style [ ("background-color", color.normal), ("height", "100%") ] ] [] ]


trackButtonStyles : Bool -> List (String, String)
trackButtonStyles isVisible =
  [ ("padding", if isVisible then "9px 4px" else "13px 8px")
  , ("box-sizing", "border-box")
  , ("width", "20px")
  , ("height", "30px")
  ]


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


viewTrack : Int -> NoteColor -> Track -> Html msg
viewTrack currentPosition color track =
  if track.isVisible then
    track.notes
      |> List.filterMap (\note ->
        if currentPosition < note.position + note.length + 5000 && currentPosition > note.position - 5000 then
          Just (Midi.toKey note, lazy2 viewNote (noteColor color note currentPosition) note)
        else
          Nothing
        )
      |> Svg.Keyed.node "g" []
  else
    Svg.Keyed.node "g" [] []


noteColor : NoteColor -> Note -> Int -> String
noteColor color note currentPosition =
  if note.position < currentPosition && currentPosition < note.position + note.length then
    color.highlight
  else
    color.normal


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

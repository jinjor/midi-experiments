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
import WebMidiApi exposing (MidiPort)


type alias Options msg =
  { onBack : msg
  , onStart : msg
  , onStop : msg
  , onToggleTrack : Int -> msg
  , onToggleConfig : msg
  , onSelectMidiOut : Int -> String -> msg
  }


type alias NoteColor =
  { highlight : String
  , normal : String
  }


colors : List NoteColor
colors =
  List.map2 NoteColor (Colors.depth 1) (Colors.depth 5)


view : Options msg -> Bool -> List MidiPort -> Bool -> Time -> Midi -> Html msg
view options showConfig midiOuts playing time midi =
  let
    currentPosition =
      Midi.timeToPosition midi.timeBase time
  in
    div [ HA.style [ "position" => "relative" ] ]
      [ midi.tracks
          |> List.map2 (viewTrack currentPosition) colors
          |> svg (containerStyles currentPosition)
      , centerLine
      , lazy3 control options midi.tracks playing
      , if showConfig then lazy3 viewConfig options midiOuts midi.tracks else H.text ""
      ]


centerLine : Html msg
centerLine =
  div
    [ HA.style
        [ "border-right" => "solid 1px #555"
        , "height" => "270px"
        , "left" => "240px"
        , "top" => "0"
        , "position" => "absolute"
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
      [ "width" => "480px"
      , "height" => "270px"
      , "background-color" => "black"
      , "display" => "block"
      ]
  ]


control : Options msg -> List Track -> Bool -> Html msg
control options tracks playing =
  div
    [ HA.style controlStyles ]
    [ backButton options
    , playButton options playing
    , trackButtons options tracks
    , configButton options
    ]


controlStyles : List (String, String)
controlStyles =
  [ "width" => "480px"
  , "height" => "30px"
  , "background-color" => "#301"
  , "display" => "flex"
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


configButton : Options msg -> Html msg
configButton options =
  controlButton
    ( onClick options.onToggleConfig )
    ( S.path [ SA.fill "#ddd", config ] [] )


trackButtons : Options msg -> List Track -> Html msg
trackButtons options tracks =
  tracks
    |> List.map2 (,) colors
    |> List.indexedMap (trackButton options)
    |> div [ HA.style [ "display" => "flex", "margin-left" => "auto", "padding-right" => "10px" ] ]


trackButton : Options msg -> Int -> (NoteColor, Track) -> Html msg
trackButton options index (color, track) =
  div
    [ onClick (options.onToggleTrack index)
    , HA.style (trackButtonStyles track.isVisible)
    ]
    [ div [ HA.style [ "background-color" => color.normal, "height" => "100%" ] ] [] ]


trackButtonStyles : Bool -> List (String, String)
trackButtonStyles isVisible =
  [ "padding" => (if isVisible then "9px 4px" else "13px 8px")
  , "box-sizing" => "border-box"
  , "width" => "20px"
  , "height" => "30px"
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


config : S.Attribute msg
config =
  SA.d "M24,11L22,13.5L22,16.5L24,19L23,20.8L19.8,20.3L17.2,21.8L16,24.8L14,24.8L12.8,21.8L10.2,20.3L7,20.8L6,19L8,16.5L8,13.5L6,11L7,9.2L10.2,9.7L12.8,8.2L14,5.2L16,5.2L17.2,8.2L19.8,9.7L23,9.2zM19,15L17.8,12.2L15,11L12.2,12.2L11,15L12.2,17.8L15,19L17.8,17.8z"


buttonStyles : List (String, String)
buttonStyles =
  [ "width" => "40px"
  , "bottom" => "0"
  , "text-align" => "center"
  ]


viewConfig : Options msg -> List MidiPort -> List Track -> Html msg
viewConfig options midiOuts tracks =
  div
    [ HA.style configStyles ]
    ( tracks
        |> List.map2 (,) colors
        |> List.indexedMap (\index (color, track) -> viewTrackConfig options midiOuts index color track)
    )


viewTrackConfig : Options msg -> List MidiPort -> Int -> NoteColor -> Track -> Html msg
viewTrackConfig options midiOuts index color track =
  div
    [ HA.style [ "display" => "flex" ]]
    [ trackButton options index (color, track)
    , WebMidiApi.viewSelect (options.onSelectMidiOut index) midiOuts track.portId
    ]


configStyles : List (String, String)
configStyles =
  [ "padding" => "10px"
  , "box-sizing" => "border-box"
  , "width" => "480px"
  , "background-color" => "#301"
  , "box-shadow" => "inset rgba(0,0,0,0.4) 0px 4px 7px"
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


(=>) = (,)


px : a -> String
px num =
  toString num ++ "px"

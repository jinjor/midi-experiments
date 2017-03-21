module Midi exposing
  ( Midi, Track, Note
  , Detailed, addDetails
  , toKey
  , fromSmf
  , positionToTime, timeToPosition
  , toggleVisibility, setMidiOut, setMidiOutToAllTracks
  )

import Time exposing (Time)
import Dict exposing (Dict)
import SmfDecoder as Smf exposing (Smf, MidiEvent(..))


type alias Midi =
  { timeBase : Int
  , tracks : List Track
  }


type alias Track =
  { channel : Int
  , name : String
  , notes : List Note
  , isVisible : Bool
  , portId : Maybe String
  }


type alias Note =
  { position : Int
  , note : Int
  , velocity : Int
  , length : Int
  }


type alias Detailed a =
  { a | track : Int, channel : Int }


addDetails : Int -> Int -> Note -> Detailed Note
addDetails track channel note =
  { position = note.position
  , note = note.note
  , velocity = note.velocity
  , length = note.length
  , channel = channel
  , track = track
  }


toKey : Note -> String
toKey note =
  toString note.position ++ toString note.note


emptyTrack : Track
emptyTrack =
  Track 0 "" [] False Nothing


fromSmf : Smf -> Midi
fromSmf smf =
  smf.tracks
    |> List.map fromSmfTrack
    |> Midi smf.header.timeBase


fromSmfTrack : Smf.Track -> Track
fromSmfTrack track =
  track.events
    |> List.foldl updateTrack (0, initContext)
    |> Tuple.second
    |> (\context ->
      Track context.channel "" (List.reverse context.notes) True Nothing
    )


updateTrack : (Int, MidiEvent) -> (Int, Context) -> (Int, Context)
updateTrack (dtime, e) (position, context) =
  ( position + dtime
  , case e of
      NoteOn ch note vel ->
        { context
          | channel = max ch context.channel
          , temporaryNotes =
              context.temporaryNotes
                |> Dict.insert note (position + dtime, vel)
        }

      NoteOff ch note ->
        { context
          | temporaryNotes =
              Dict.remove note context.temporaryNotes
          , notes =
              Dict.get note context.temporaryNotes
                |> Maybe.map (\(startPos, vel) ->
                    Note startPos note vel (position + dtime - startPos)
                      :: context.notes
                  )
                |> Maybe.withDefault context.notes
        }

      _ ->
        context
  )


type alias Context =
  { channel : Int
  , temporaryNotes : Dict Int (Int, Int)
  , notes : List Note
  }


initContext : Context
initContext =
  Context 0 Dict.empty []


positionToTime : Int -> Int -> Time
positionToTime timeBase position =
  toFloat position * (1000.0 / (toFloat timeBase * 2.0))


timeToPosition : Int -> Time -> Int
timeToPosition timeBase time =
  floor <| (toFloat timeBase * 2.0 / 1000) * time


toggleVisibility : Int -> Midi -> Midi
toggleVisibility index midi =
  { midi
    | tracks =
        midi.tracks
          |> List.indexedMap (\i track ->
              if i == index then
                { track | isVisible = not track.isVisible }
              else
                track
            )
  }


setMidiOutToAllTracks : String -> Midi -> Midi
setMidiOutToAllTracks portId midi =
  { midi
    | tracks =
        midi.tracks
          |> List.map (\track -> { track | portId = Just portId })
  }


setMidiOut : Int -> String -> Midi -> Midi
setMidiOut index portId midi =
  { midi
    | tracks =
        midi.tracks
          |> List.indexedMap (\i track ->
              if i == index then
                { track | portId = Just portId }
              else
                track
            )
  }

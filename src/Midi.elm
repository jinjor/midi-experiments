module Midi exposing (Midi, Track, Note, Channeled, addChannel, toKey, fromSmf)

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
  }


type alias Note =
  { position : Int
  , note : Int
  , velocity : Int
  , length : Int
  }


type alias Channeled a =
  { a | channel : Int }


addChannel : Int -> Note -> Channeled Note
addChannel channel note =
  { position = note.position
  , note = note.note
  , velocity = note.velocity
  , length = note.length
  , channel = channel
  }


toKey : Note -> String
toKey note =
  toString note.position ++ toString note.note


emptyTrack : Track
emptyTrack =
  Track 0 "" []


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
      Track context.channel "" (List.reverse context.notes)
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

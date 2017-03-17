module Midi exposing (Midi, Track, Note, toKey, fromSmf)

import Dict exposing (Dict)
import SmfDecoder as Smf exposing (Smf, MidiEvent(..))


type alias Midi =
  { timeBase : Int
  , tracks : List Track
  }


type alias Track =
  { name : String
  , notes : List Note
  }


type alias Note =
  { position : Int
  , note : Int
  , length : Int
  }


toKey : Note -> String
toKey note =
  toString note.position ++ toString note.note


emptyTrack : Track
emptyTrack =
  Track "" []


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
    |> .notes
    |> List.reverse
    |> (\notes -> Track "" notes)


updateTrack : (Int, MidiEvent) -> (Int, Context) -> (Int, Context)
updateTrack (dtime, e) (position, context) =
  ( position + dtime
  , case e of
      NoteOn ch note vel ->
        { context
          | temporaryNotes =
              Dict.insert note (position + dtime, vel) context.temporaryNotes
        }

      NoteOff ch note ->
        { context
          | temporaryNotes =
              Dict.remove note context.temporaryNotes
          , notes =
              Dict.get note context.temporaryNotes
                |> Maybe.map (\(startPos, vel) ->
                    Note startPos note (position + dtime - startPos)
                      :: context.notes
                  )
                |> Maybe.withDefault context.notes
        }

      _ ->
        context
  )


type alias Context =
  { temporaryNotes : Dict Int (Int, Int)
  , notes : List Note
  }


initContext : Context
initContext =
  Context Dict.empty []

module Allocator.Codecs exposing (..)

import Allocator.Types exposing (..)
import Codec
import Generic.Json
import Generic.Yaml
import Json.Decode
import Yaml.Decode


decodeSaved : { savedAsString : String, dataFormat : DataFormat } -> Result String (Saved {})
decodeSaved { savedAsString, dataFormat } =
    case dataFormat of
        Json _ ->
            Codec.decodeString codecSaved savedAsString
                |> Result.mapError Json.Decode.errorToString

        Yaml _ ->
            savedAsString
                |> Generic.Yaml.decode
                |> Result.map Generic.Json.encode
                |> (\res ->
                        case res of
                            Err err ->
                                Err (Yaml.Decode.errorToString err)

                            Ok ok ->
                                case Codec.decodeValue codecSaved ok of
                                    Err err2 ->
                                        Err (Json.Decode.errorToString err2)

                                    Ok ok2 ->
                                        Ok ok2
                   )


defaultSavedDataformat : DataFormat
defaultSavedDataformat =
    Yaml 2


encodeSavedToString : Saved a -> DataFormat -> String
encodeSavedToString saved dataFormat =
    { people = saved.people
    , projects = saved.projects
    }
        |> (\saved_ ->
                case dataFormat of
                    Json indentation ->
                        saved_
                            |> Codec.encodeToString indentation codecSaved

                    Yaml indentation ->
                        saved_
                            |> Codec.encodeToValue codecSaved
                            |> Generic.Json.decodeValue
                            |> Generic.Yaml.encode
                            |> Generic.Yaml.toString indentation
           )


decoderMousePosition : Json.Decode.Decoder { x : Float, y : Float }
decoderMousePosition =
    Json.Decode.map2 (\a b -> { x = a, y = b })
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)


codecSaved : Codec.Codec (Saved {})
codecSaved =
    Codec.object
        (\v1 v2 ->
            { people = v1
            , projects = v2
            }
        )
        |> Codec.field "people" .people (Codec.list codecPerson)
        |> Codec.field "projects" .projects (Codec.list codecProject)
        |> Codec.buildObject


codecProject : Codec.Codec Project
codecProject =
    Codec.object
        (\v1 v2 v3 v4 v5 v6 ->
            { name = v1
            , id = v2
            , color = v3
            , x = v4
            , y = v5
            , hidden = v6
            }
        )
        |> Codec.field "name" .name Codec.string
        |> Codec.field "id" .id Codec.string
        |> Codec.field "color" .color codecColor
        |> Codec.field "x" .x Codec.float
        |> Codec.field "y" .y Codec.float
        |> Codec.field "hidden" .hidden Codec.bool
        |> Codec.buildObject


codecColor : Codec.Codec ColorType
codecColor =
    Codec.triple Codec.int Codec.int Codec.int


codecPerson : Codec.Codec Person
codecPerson =
    Codec.object
        (\v1 v2 v3 v4 v5 v6 v7 ->
            { id = v1
            , x = v2
            , y = v3
            , name = v4
            , allocations = v5
            , hidden = v6
            , color = v7
            }
        )
        |> Codec.field "id" .id Codec.string
        |> Codec.field "x" .x Codec.float
        |> Codec.field "y" .y Codec.float
        |> Codec.field "name" .name Codec.string
        |> Codec.field "allocations" .allocations (Codec.list codecAllocation)
        |> Codec.field "hidden" .hidden Codec.bool
        |> Codec.field "color" .color codecColor
        |> Codec.buildObject


codecAllocation : Codec.Codec Allocation
codecAllocation =
    Codec.tuple Codec.string <| Codec.list <| Codec.tuple Codec.string Codec.float


decodeButtons : Json.Decode.Decoder Bool
decodeButtons =
    --
    -- From https://github.com/elm/browser/blob/1.0.2/examples/src/Drag.elm#L223C1-L234C61
    --
    -- What happens when the user is dragging, but the "mouse up" occurs outside
    -- the browser window? We need to stop listening for mouse movement and end the
    -- drag. We use MouseEvent.buttons to detect this:
    --
    --     https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/buttons
    --
    -- The "buttons" value is 1 when "left-click" is pressed, so we use that to
    -- detect zombie drags.
    --
    Json.Decode.field "buttons" (Json.Decode.map (\buttons -> buttons == 1) Json.Decode.int)


decoderForDragMove : Json.Decode.Decoder Msg
decoderForDragMove =
    Json.Decode.map2 DragMove decodeButtons decoderMousePosition


decoderForDragStop : Json.Decode.Decoder Msg
decoderForDragStop =
    Json.Decode.map DragStop decoderMousePosition


decoderForDragStart : Object -> Json.Decode.Decoder Msg
decoderForDragStart object =
    Json.Decode.map (DragStart object) decoderMousePosition

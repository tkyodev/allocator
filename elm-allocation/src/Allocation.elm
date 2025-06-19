module Allocation exposing (main)

-- https://strapi.io/
-- https://jsoneditoronline.org/
-- https://jsonsilo.com/

import Allocation.Confluence
import Allocation.Types exposing (..)
import Allocation.View
import Browser
import Browser.Events
import Codec
import Dict
import Generic.Json
import Generic.Yaml
import Html
import Http
import Iso8601
import Json.Decode
import Process
import Task
import Time
import Url
import Yaml.Decode


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        m : Saved {}
        m =
            flags.saved
                |> Generic.Yaml.decode
                |> Result.map Generic.Json.encode
                |> (\res ->
                        case res of
                            Err err ->
                                Err (Yaml.Decode.errorToString err)

                            Ok ok ->
                                case Codec.decodeValue Allocation.View.codecSaved ok of
                                    Err err2 ->
                                        Err (Json.Decode.errorToString err2)

                                    Ok ok2 ->
                                        Ok ok2
                   )
                |> Result.withDefault defaultSaved
    in
    ( { people = m.people
      , x = 0
      , y = 0
      , scale = 0.7
      , projects = m.projects
      , state = AllInCenter
      , showSaved = False
      , initialFlags = flags
      , posix = Time.millisToPosix flags.posix
      , currentDate =
            flags.posix
                |> Time.millisToPosix
                |> Iso8601.fromTime
                |> String.left 10
      , dashboardStatus = Default
      , dataFormat = Json 2
      , dragState = Static
      , url =
            flags.href
                |> Url.fromString
                |> Maybe.withDefault
                    { protocol = Url.Https
                    , host = ""
                    , port_ = Nothing
                    , path = ""
                    , query = Nothing
                    , fragment = Nothing
                    }
      , saveStatus = NotRequested
      , gridSize = 20
      , gridSnap = True
      , savedCounter = 0
      }
    , Task.perform (\_ -> ChangeState OwnPosition) (Process.sleep 50)
    )


defaultSaved : Saved {}
defaultSaved =
    { people =
        [ { id = "p01"
          , name = "Person A"
          , color = palette.lightBlue
          , allocations =
                [ ( "2025-06-01"
                  , [ ( "prj1", 70 )
                    , ( "prj4", 30 )
                    ]
                  )
                , ( "2025-06-17"
                  , [ ( "prj1", 30 )
                    , ( "prj3", 70 )
                    ]
                  )
                ]
          , x = 86
          , y = 77
          , hidden = False
          }
        , { id = "p02"
          , name = "Person B"
          , color = palette.orange
          , allocations =
                [ ( "2025-06-16"
                  , [ ( "prj2", 10 )
                    , ( "prj3", 20 )
                    , ( "prj4", 30 )
                    , ( "prj5", 40 )
                    ]
                  )
                ]
          , x = 378
          , y = 71
          , hidden = False
          }
        , { id = "p03"
          , name = "Person C"
          , color = palette.limeGreen
          , allocations =
                [ ( "2025-06-16"
                  , [ ( "prj1", 20 )
                    , ( "prj2", 60 )
                    , ( "prj4", 20 )
                    ]
                  )
                ]
          , x = 687
          , y = 72
          , hidden = False
          }
        , { id = "p04"
          , name = "Person D"
          , color = palette.coralLightRed
          , allocations =
                [ ( "2025-06-16"
                  , [ ( "prj1", 30 )
                    , ( "prj2", 20 )
                    , ( "prj3", 10 )
                    , ( "prj4", 30 )
                    , ( "prj5", 10 )
                    ]
                  )
                ]
          , x = 1010
          , y = 80
          , hidden = False
          }
        ]
    , projects =
        [ { name = "Project A"
          , id = "prj1"
          , color = palette.lightBlue
          , x = 82
          , y = 563
          , hidden = False
          }
        , { name = "Project B"
          , id = "prj2"
          , color = palette.orange
          , x = 396
          , y = 563
          , hidden = False
          }
        , { name = "Project C"
          , id = "prj3"
          , color = palette.darkerBlue
          , x = 710
          , y = 555
          , hidden = False
          }
        , { name = "Project D"
          , id = "prj4"
          , color = palette.coralLightRed
          , x = 1021
          , y = 563
          , hidden = False
          }
        , { name = "Project E"
          , id = "prj5"
          , color = palette.limeGreen
          , x = 1320
          , y = 566
          , hidden = False
          }
        ]
    }


palette :
    { coralLightRed : String
    , darkerBlue : String
    , lightBlue : String
    , limeGreen : String
    , orange : String
    }
palette =
    { lightBlue = "rgba(77, 199, 255, 0.7)"
    , orange = "rgba(255, 153, 51, 0.7)"
    , darkerBlue = "rgba(50, 120, 200, 0.7)"
    , coralLightRed = "rgba(255, 80, 80, 0.7)"
    , limeGreen = "rgba(102, 204, 0, 0.7)"
    }


data : String
data =
    """
<!-- Confluence MACRO START -->
<allocation-app></allocation-app>
<script>
    dataMacroId = "{{dataMacroId}}";
    saved = `
{{saved}}
`;
</script>
<script src="https://tkyodev.github.io/allocation/allocation.min.js"></script>
<!-- Confluence MACRO END -->
"""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveResponse result ->
            ( { model
                | saveStatus = Saved result
                , savedCounter =
                    case result of
                        Ok _ ->
                            model.savedCounter + 1

                        Err _ ->
                            model.savedCounter
              }
            , Cmd.none
            )

        Save ->
            case model.initialFlags.maybeConfluenceData of
                Nothing ->
                    ( model, Cmd.none )

                Just confluenceData ->
                    ( model
                    , Allocation.Confluence.request
                        { confluenceData = confluenceData
                        , content = "TODO"
                        , msgResponse = SaveResponse
                        , url = model.url
                        , savedCounter = model.savedCounter
                        }
                    )

        DragStart object position ->
            let
                common : Common {}
                common =
                    Allocation.View.objectToCommon object
            in
            ( { model
                | dragState =
                    Moving
                        { object = object
                        , initialPersonXY = { x = common.x, y = common.y }
                        , initialDragXY = { x = position.x, y = position.y }
                        }
              }
            , Cmd.none
            )

        DragMove isDown position ->
            if isDown then
                ( updateObject position model, Cmd.none )

            else
                ( { model | dragState = Static }, Cmd.none )

        DragStop fraction ->
            ( { model | dragState = Static }, Cmd.none )

        ChangeGrid gridSize ->
            ( { model | gridSize = gridSize }, Cmd.none )

        ChangeCommonData object common ->
            let
                changeCommon : Common a -> Common a
                changeCommon p =
                    if p.id == common.id then
                        { p
                            | name = common.name
                            , x = common.x
                            , y = common.y
                            , hidden = common.hidden
                        }

                    else
                        p
            in
            case object of
                Project _ ->
                    ( { model | projects = List.map changeCommon model.projects }, Cmd.none )

                Person _ ->
                    ( { model | people = List.map changeCommon model.people }, Cmd.none )

        ChangeRatio args ->
            ( updateRatio args model, Cmd.none )

        ChangeDashboardStatus dashboardStatus ->
            ( { model | dashboardStatus = dashboardStatus }, Cmd.none )

        ToggleDataVisibility ->
            ( { model | showSaved = not model.showSaved }, Cmd.none )

        ToggleSnapToGrid ->
            ( { model | gridSnap = not model.gridSnap }, Cmd.none )

        ToggleDataFormat ->
            ( { model
                | dataFormat =
                    case model.dataFormat of
                        Json indentation ->
                            Yaml indentation

                        Yaml indentation ->
                            Json indentation
              }
            , Cmd.none
            )

        Reset ->
            init model.initialFlags

        ChangeState state ->
            ( { model | state = state }, Cmd.none )

        ChangeScale scale ->
            ( { model | scale = scale }, Cmd.none )

        ChangeX x ->
            ( { model | x = x }, Cmd.none )

        ChangeY y ->
            ( { model | y = y }, Cmd.none )


updateObject : { x : Float, y : Float } -> Model -> Model
updateObject position model =
    case model.dragState of
        Static ->
            model

        Moving { object, initialPersonXY, initialDragXY } ->
            let
                moveObjectHelper : Id -> Common a -> Common a
                moveObjectHelper projectOrPersonId obj =
                    if obj.id == projectOrPersonId then
                        let
                            objectMovedX : Float
                            objectMovedX =
                                initialPersonXY.x + ((position.x - initialDragXY.x) / model.scale)

                            objectMovedY : Float
                            objectMovedY =
                                initialPersonXY.y + ((position.y - initialDragXY.y) / model.scale)
                        in
                        if model.gridSnap then
                            { obj
                                | x = toFloat (round (objectMovedX / toFloat model.gridSize) * model.gridSize)
                                , y = toFloat (round (objectMovedY / toFloat model.gridSize) * model.gridSize)
                            }

                        else
                            { obj
                                | x = toFloat (round objectMovedX)
                                , y = toFloat (round objectMovedY)
                            }

                    else
                        obj
            in
            case object of
                Project projectOrPerson ->
                    { model | projects = List.map (moveObjectHelper projectOrPerson.id) model.projects }

                Person projectOrPerson ->
                    { model | people = List.map (moveObjectHelper projectOrPerson.id) model.people }


updateRatio :
    { dateAsString : DateAsString
    , personId : Id
    , projectId : String
    , ratio : String
    }
    -> Model
    -> Model
updateRatio { personId, dateAsString, projectId, ratio } model =
    case
        ( Allocation.View.personIdToPerson personId model.people
        , String.toFloat
            (if ratio == "" then
                "0"

             else
                ratio
            )
        )
    of
        ( Just person, Just ratioAsFloat ) ->
            let
                -- In case the "dateAsString" allocation are not created yet, we use the
                -- closest as template.
                result : LatestAllocation
                result =
                    Allocation.View.getLatestAllocation dateAsString person.allocations

                newAllocation : List ( Id, Ratio )
                newAllocation =
                    result.allocation
                        |> Dict.fromList
                        |> Dict.update projectId
                            (\v ->
                                if ratioAsFloat == 0 then
                                    -- If the value is 0 we completely remove
                                    -- this record
                                    Nothing

                                else
                                    Just ratioAsFloat
                            )
                        |> Dict.toList

                allocations =
                    person.allocations
                        |> Dict.fromList
                        |> Dict.update dateAsString
                            (\v ->
                                if List.isEmpty newAllocation then
                                    -- If the list is empty we completely remove
                                    -- this record
                                    Nothing

                                else
                                    Just newAllocation
                            )
                        |> Dict.toList
            in
            { model
                | people =
                    List.map
                        (\p ->
                            if p.id == person.id then
                                { p | allocations = allocations }

                            else
                                p
                        )
                        model.people
            }

        _ ->
            model


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = Allocation.View.view
        , update = update
        , subscriptions =
            \model ->
                case model.dragState of
                    Static ->
                        Sub.none

                    Moving _ ->
                        Sub.batch
                            [ Browser.Events.onMouseMove (Json.Decode.map2 DragMove decodeButtons Allocation.View.decoderMousePosition)
                            , Browser.Events.onMouseUp (Json.Decode.map DragStop Allocation.View.decoderMousePosition)
                            ]
        }


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

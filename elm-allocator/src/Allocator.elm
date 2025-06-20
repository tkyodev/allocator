module Allocator exposing (main)

-- https://strapi.io/
-- https://jsoneditoronline.org/
-- https://jsonsilo.com/

import Allocator.Codecs
import Allocator.Confluence
import Allocator.DataManipulation
import Allocator.Types exposing (..)
import Allocator.View
import Browser
import Browser.Events
import Dict
import Iso8601
import Process
import Task
import Time
import Url


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        saved : Saved {}
        saved =
            Allocator.Codecs.decodeSaved
                { savedAsString = flags.saved
                , dataFormat = Allocator.Codecs.defaultSavedDataformat
                }
                |> Result.withDefault defaultSaved
    in
    ( { people = saved.people
      , x = 0
      , y = 0
      , scale = 0.7
      , projects = saved.projects
      , state = AllInCenter
      , showCharts = True
      , flags = flags
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
      , latestSavedAsYaml = flags.saved
      }
    , Task.perform (\_ -> ChangeState OwnPosition) (Process.sleep 100)
    )


defaultSaved : Saved {}
defaultSaved =
    let
        spacePeople : number
        spacePeople =
            480

        spaceProject : number
        spaceProject =
            280
    in
    { people =
        [ { id = "p01"
          , name = "Person A"
          , color = palette.lightBlue
          , allocations =
                [ ( "2025-06-01", [ ( "prj1", 30 ), ( "prj4", 70 ) ] )
                , ( "2025-06-04", [ ( "prj1", 30 ), ( "prj2", 20 ), ( "prj4", 50 ) ] )
                , ( "2025-06-06", [ ( "prj1", 10 ), ( "prj2", 30 ), ( "prj4", 60 ) ] )
                , ( "2025-06-08", [ ( "prj1", 10 ), ( "prj3", 90 ) ] )
                , ( "2025-06-10", [ ( "prj1", 40 ), ( "prj3", 60 ) ] )
                , ( "2025-06-17", [ ( "prj1", 50 ), ( "prj3", 40 ), ( "prj5", 10 ) ] )
                ]
          , x = 80
          , y = 80
          , hidden = False
          }
        , { id = "p02"
          , name = "Person B"
          , color = palette.orange
          , allocations =
                [ ( "2025-06-10", [ ( "prj2", 5 ), ( "prj3", 25 ), ( "prj4", 30 ), ( "prj5", 40 ) ] )
                , ( "2025-06-15", [ ( "prj2", 10 ), ( "prj3", 20 ), ( "prj4", 30 ), ( "prj5", 40 ) ] )
                , ( "2025-06-20", [ ( "prj2", 20 ), ( "prj3", 50 ), ( "prj4", 30 ) ] )
                ]
          , x = 80 + spacePeople
          , y = 80
          , hidden = False
          }
        , { id = "p03"
          , name = "Person C"
          , color = palette.limeGreen
          , allocations =
                [ ( "2025-05-16", [ ( "prj1", 50 ), ( "prj2", 30 ), ( "prj4", 20 ) ] )
                , ( "2025-06-16", [ ( "prj1", 20 ), ( "prj2", 60 ), ( "prj4", 20 ) ] )
                ]
          , x = 80 + (spacePeople * 2)
          , y = 80
          , hidden = False
          }
        ]
    , projects =
        [ { name = "Project A"
          , id = "prj1"
          , color = palette.lightBlue
          , x = 80
          , y = 540
          , hidden = False
          }
        , { name = "Project B"
          , id = "prj2"
          , color = palette.orange
          , x = 80 + spaceProject
          , y = 540
          , hidden = False
          }
        , { name = "Project C"
          , id = "prj3"
          , color = palette.darkerBlue
          , x = 80 + (spaceProject * 2)
          , y = 540
          , hidden = False
          }
        , { name = "Project D"
          , id = "prj4"
          , color = palette.coralLightRed
          , x = 80 + (spaceProject * 3)
          , y = 540
          , hidden = False
          }
        , { name = "Project E"
          , id = "prj5"
          , color = palette.limeGreen
          , x = 80 + (spaceProject * 4)
          , y = 540
          , hidden = False
          }
        ]
    }


palette :
    { coralLightRed : ColorType
    , darkerBlue : ColorType
    , lightBlue : ColorType
    , limeGreen : ColorType
    , orange : ColorType
    }
palette =
    { lightBlue = ( 77, 199, 255 )
    , orange = ( 255, 153, 51 )
    , darkerBlue = ( 50, 120, 200 )
    , coralLightRed = ( 255, 80, 80 )
    , limeGreen = ( 102, 204, 0 )
    }


update2 : Msg -> Model -> ( Model, Cmd Msg )
update2 _ model =
    case model.dashboardStatus of
        EditingData { error } ->
            case error of
                Nothing ->
                    ( { model
                        | dashboardStatus =
                            EditingData
                                { dataAsString = Allocator.Codecs.encodeSavedToString model model.dataFormat
                                , error = Nothing
                                }
                      }
                    , Cmd.none
                    )

                Just _ ->
                    -- It is in error mode, we do not overwrite it
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditData dataAsString ->
            case Allocator.Codecs.decodeSaved { savedAsString = dataAsString, dataFormat = model.dataFormat } of
                Err err ->
                    ( { model
                        | dashboardStatus =
                            EditingData
                                { dataAsString = dataAsString
                                , error = Just err
                                }
                      }
                    , Cmd.none
                    )

                Ok ok ->
                    ( { model
                        | dashboardStatus =
                            EditingData
                                { dataAsString = dataAsString
                                , error = Nothing
                                }
                        , people = ok.people
                        , projects = ok.projects
                      }
                    , Cmd.none
                    )

        ToggleEditData ->
            ( { model
                | dashboardStatus =
                    case model.dashboardStatus of
                        EditingData _ ->
                            Default

                        _ ->
                            EditingData
                                { dataAsString =
                                    Allocator.Codecs.encodeSavedToString
                                        model
                                        model.dataFormat
                                , error = Nothing
                                }
              }
            , Cmd.none
            )

        SaveResponse { savedAsYaml } result ->
            ( { model
                | saveStatus = Saved result
                , savedCounter =
                    case result of
                        Ok _ ->
                            model.savedCounter + 1

                        Err _ ->
                            model.savedCounter
                , latestSavedAsYaml =
                    case result of
                        Ok _ ->
                            savedAsYaml

                        Err _ ->
                            model.latestSavedAsYaml
              }
            , Cmd.none
            )

        Save ->
            case model.flags.maybeConflData of
                Nothing ->
                    ( model, Cmd.none )

                Just confluenceData ->
                    ( model
                    , Allocator.Confluence.request
                        { confluenceData = confluenceData
                        , saved = model
                        , msgResponse = SaveResponse
                        , url = model.url
                        , savedCounter = model.savedCounter
                        , meta = model.flags.meta
                        }
                    )

        DragStart object position ->
            let
                common : Common {}
                common =
                    Allocator.DataManipulation.objectToCommon object
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

        DragStop _ ->
            ( { model | dragState = Static }, Cmd.none )

        ChangeGrid gridSize ->
            ( { model | gridSize = gridSize }, Cmd.none )

        ChangeCommonData object common ->
            let
                changeObject : Common a -> Common a
                changeObject obj =
                    if obj.id == common.id then
                        { obj
                            | name = common.name
                            , x = common.x
                            , y = common.y
                            , hidden = common.hidden
                        }

                    else
                        obj
            in
            case object of
                Project _ ->
                    ( { model | projects = List.map changeObject model.projects }, Cmd.none )

                Person _ ->
                    ( { model | people = List.map changeObject model.people }, Cmd.none )

        ChangeRatio args ->
            ( updateRatio args model, Cmd.none )

        ChangeDashboardStatus dashboardStatus ->
            ( { model | dashboardStatus = dashboardStatus }, Cmd.none )

        ToggleCharts ->
            ( { model | showCharts = not model.showCharts }, Cmd.none )

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
            init model.flags

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


personIdToPerson : Id -> List Person -> Maybe Person
personIdToPerson personId people =
    people
        |> List.filter (\p -> p.id == personId)
        |> List.head


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
        ( personIdToPerson personId model.people
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
                latestAllocation : LatestAllocation
                latestAllocation =
                    Allocator.DataManipulation.getLatestAllocation
                        dateAsString
                        person.allocations

                newAllocation : List ( ProjectId, Ratio )
                newAllocation =
                    latestAllocation.ratiosByProjectId
                        |> Dict.fromList
                        |> Dict.update projectId
                            (\_ ->
                                if ratioAsFloat == 0 then
                                    -- If the value is 0 we completely remove
                                    -- this record
                                    Nothing

                                else
                                    Just ratioAsFloat
                            )
                        |> Dict.toList

                allocations : List Allocation
                allocations =
                    person.allocations
                        |> Dict.fromList
                        |> Dict.update dateAsString
                            (\_ ->
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
        , view = Allocator.View.view
        , update =
            \msg model ->
                update msg model
                    |> (\( model2, cmd2 ) ->
                            ( update2 msg model2, cmd2 )
                       )
                    |> (\( ( model3, cmd3 ), cmd2 ) ->
                            ( model3, Cmd.batch [ cmd2, cmd3 ] )
                       )
        , subscriptions =
            \model ->
                case model.dragState of
                    Static ->
                        Sub.none

                    Moving _ ->
                        Sub.batch
                            [ Browser.Events.onMouseMove Allocator.Codecs.decoderForDragMove
                            , Browser.Events.onMouseUp Allocator.Codecs.decoderForDragStop
                            ]
        }

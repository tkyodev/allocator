module Allocation.View exposing (..)

import Allocation.Components.Slider
import Allocation.Components.Switch
import Allocation.Types exposing (..)
import Codec
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generic.Json
import Generic.Yaml
import Html
import Html.Attributes
import Html.Events
import Iso8601
import Json.Decode
import Svg
import Svg.Attributes
import Time


view : Model -> Html.Html Msg
view model =
    let
        latestAllocationsByPersonId : Dict.Dict PersonId LatestAllocation
        latestAllocationsByPersonId =
            model.people
                |> List.map
                    (\person ->
                        let
                            result : LatestAllocation
                            result =
                                getLatestAllocation model.currentDate person.allocations
                        in
                        ( person.id, result )
                    )
                |> Dict.fromList

        latestAllocationsByProjectId : Dict.Dict ProjectId (List ( PersonId, Ratio ))
        latestAllocationsByProjectId =
            latestAllocationsByPersonId
                |> Dict.foldl
                    (\personId { latest, allocation } acc ->
                        List.foldl
                            (\( projectId, ratio ) acc2 ->
                                Dict.update projectId
                                    (\maybeV ->
                                        case maybeV of
                                            Just v ->
                                                Just <| ( personId, ratio ) :: v

                                            Nothing ->
                                                Just <| ( personId, ratio ) :: []
                                    )
                                    acc2
                            )
                            acc
                            allocation
                    )
                    Dict.empty
    in
    layout_new
        ([ viewDashboard model
         , Font.size 16
         ]
            ++ (case model.dragState of
                    Static ->
                        []

                    Moving _ ->
                        [ htmlAttribute <| Html.Attributes.style "user-select" "none" ]
               )
        )
        (column
            ([ moveRight -model.x
             , moveDown -model.y
             , width fill
             ]
                ++ viewObjectWrapper (List.map (\person -> Person person) model.people) latestAllocationsByPersonId latestAllocationsByProjectId model
                ++ viewObjectWrapper (List.map (\project -> Project project) model.projects) latestAllocationsByPersonId latestAllocationsByProjectId model
            )
            (if model.showSaved then
                [ el [ padding 10, Background.color <| rgba 0 0 0 0.1 ] <|
                    html <|
                        Html.pre []
                            [ Html.text
                                ({ people = model.people
                                 , projects = model.projects
                                 }
                                    |> (\data ->
                                            case model.dataFormat of
                                                Json indentation ->
                                                    data
                                                        |> Codec.encodeToString indentation codecSaved

                                                Yaml indentation ->
                                                    data
                                                        |> Codec.encodeToValue codecSaved
                                                        |> Generic.Json.decodeValue
                                                        |> Generic.Yaml.encode
                                                        |> Generic.Yaml.toString indentation
                                       )
                                )
                            ]
                ]

             else
                [ html <| Html.pre [] [] ]
            )
        )


viewDashboard : Model -> Attribute Msg
viewDashboard model =
    inFront <|
        column
            [ padding 10
            , spacing 8
            , alignRight
            , Background.color <| rgba 1 1 1 0.4
            , Font.family [ Font.monospace ]
            , Font.size 14
            , moveDown 100
            ]
            (case model.dashboardStatus of
                Default ->
                    viewDashboardDefault model

                EditingPerson personId dateAsString ->
                    viewDashboardEditingPerson personId dateAsString model

                EditingProject projectId dateAsString ->
                    [ text "TODO EditingProject" ]
            )


viewObjectWrapper :
    List Object
    -> Dict.Dict Id LatestAllocation
    -> Dict.Dict ProjectId (List ( PersonId, Ratio ))
    -> Model
    -> List (Attribute Msg)
viewObjectWrapper objects latestAllocationsByPersonId latestAllocationsByProjectId model =
    List.map
        (\object ->
            let
                common : Common {}
                common =
                    objectToCommon object
            in
            inFront <|
                row
                    ((case model.state of
                        AllInCenter ->
                            [ moveRight 0
                            , moveDown 0
                            ]

                        OwnPosition ->
                            [ moveRight (common.x * model.scale)
                            , moveDown (common.y * model.scale)
                            ]
                                ++ (case model.dragState of
                                        Static ->
                                            [ htmlAttribute <| Html.Attributes.style "transition" "all 600ms cubic-bezier(0.22, 1, 0.36, 1)" ]

                                        Moving _ ->
                                            []
                                   )
                     )
                        ++ [ spacing (round (5 * model.scale))
                           , htmlAttribute <|
                                Html.Attributes.style "cursor"
                                    (case model.dragState of
                                        Static ->
                                            "grab"

                                        Moving _ ->
                                            "grabbing"
                                    )
                           , htmlAttribute <| Html.Events.on "mousedown" (Json.Decode.map (DragStart object) decoderMousePosition)
                           ]
                    )
                    (viewObject latestAllocationsByPersonId latestAllocationsByProjectId model.scale object model)
        )
        objects


viewDashboardEditingPerson : Id -> DateAsString -> Model -> List (Element Msg)
viewDashboardEditingPerson personId dateAsString model =
    case personIdToPerson personId model.people of
        Just person ->
            let
                result : LatestAllocation
                result =
                    getLatestAllocation dateAsString person.allocations
            in
            [ Input.button attrsButton
                { onPress = Just <| ChangeDashboardStatus Default
                , label = text "close"
                }
            , Input.text [ padding 3, width <| px 150 ]
                { onChange =
                    \name ->
                        ChangeCommonData (Person person)
                            { id = person.id
                            , name = name
                            , x = person.x
                            , y = person.y
                            , hidden = person.hidden
                            , color = person.color
                            }
                , text = person.name
                , placeholder = Nothing
                , label = Input.labelLeft [] <| text "Name"
                }
            ]
                ++ List.map
                    (\project ->
                        let
                            alloc =
                                result.allocation
                                    |> List.filter (\( projectId, ratio ) -> projectId == project.id)
                                    |> List.head
                                    |> Maybe.map Tuple.second
                                    |> Maybe.withDefault 0
                        in
                        row [ spacing 10 ]
                            [ el
                                [ width <| px 20
                                , height <| px 20
                                , htmlAttribute <| Html.Attributes.style "background-color" project.color
                                ]
                                none
                            , el [ width <| px 100 ] <| text <| project.name
                            , Input.text [ Font.alignRight, width <| px 40, padding 3 ]
                                { label = Input.labelRight [] (text "%")
                                , onChange =
                                    \ratio ->
                                        ChangeRatio
                                            { personId = personId
                                            , dateAsString = dateAsString
                                            , projectId = project.id
                                            , ratio = ratio
                                            }
                                , placeholder = Nothing
                                , text = String.fromFloat alloc
                                }
                            ]
                    )
                    model.projects
                ++ [ row [ spacing 10 ]
                        [ Allocation.Components.Slider.view
                            { inputFieldsWidth = 100
                            , labelWidth = 20
                            , label = "x"
                            , max = 2900
                            , min = -300
                            , step = Nothing
                            , value = person.x
                            , msg =
                                \x ->
                                    ChangeCommonData (Person person)
                                        { id = person.id
                                        , name = person.name
                                        , x = x
                                        , y = person.y
                                        , hidden = person.hidden
                                        , color = person.color
                                        }
                            }
                        , Input.text [ Font.alignRight, width <| px 45, padding 3 ]
                            { label = Input.labelRight [] (text "px")
                            , onChange =
                                \string ->
                                    ChangeCommonData (Person person)
                                        { id = person.id
                                        , name = person.name
                                        , x = Maybe.withDefault 0 (String.toFloat string)
                                        , y = person.y
                                        , hidden = person.hidden
                                        , color = person.color
                                        }
                            , placeholder = Nothing
                            , text = String.fromInt (round person.x)
                            }
                        ]
                   , row [ spacing 10 ]
                        [ Allocation.Components.Slider.view
                            { inputFieldsWidth = 100
                            , labelWidth = 20
                            , label = "y"
                            , max = 1600
                            , min = -400
                            , step = Just 1
                            , value = person.y
                            , msg =
                                \y ->
                                    ChangeCommonData (Person person)
                                        { id = person.id
                                        , name = person.name
                                        , x = person.x
                                        , y = y
                                        , hidden = person.hidden
                                        , color = person.color
                                        }
                            }
                        , Input.text [ Font.alignRight, width <| px 45, padding 3 ]
                            { label = Input.labelRight [] (text "px")
                            , onChange =
                                \string ->
                                    ChangeCommonData (Person person)
                                        { id = person.id
                                        , name = person.name
                                        , x = person.x
                                        , y = Maybe.withDefault 0 (String.toFloat string)
                                        , hidden = person.hidden
                                        , color = person.color
                                        }
                            , placeholder = Nothing
                            , text = String.fromInt (round person.y)
                            }
                        ]
                   ]

        Nothing ->
            [ text <| "personId " ++ personId ++ " does not exists." ]


viewDashboardDefault : Model -> List (Element Msg)
viewDashboardDefault model =
    [ Allocation.Components.Slider.view
        { inputFieldsWidth = 100
        , labelWidth = 50
        , label = "x"
        , max = 1000
        , min = 0
        , step = Nothing
        , value = model.x
        , msg = ChangeX
        }
    , Allocation.Components.Slider.view
        { inputFieldsWidth = 100
        , labelWidth = 50
        , label = "y"
        , max = 1000
        , min = 0
        , step = Nothing
        , value = model.y
        , msg = ChangeY
        }
    , Allocation.Components.Slider.view
        { inputFieldsWidth = 100
        , labelWidth = 50
        , label = "scale"
        , max = 5
        , min = 0.5
        , step = Nothing
        , value = model.scale
        , msg = ChangeScale
        }
    , row [ spacing 10 ]
        [ Allocation.Components.Switch.view ToggleSnapToGrid model.gridSnap "Snap to grid"
        , Input.text [ padding 3, width <| px 28, Font.alignRight ]
            { onChange =
                \string ->
                    case String.toInt string of
                        Nothing ->
                            ChangeGrid model.gridSize

                        Just gridSize ->
                            ChangeGrid gridSize
            , text = String.fromInt model.gridSize
            , placeholder = Nothing
            , label = Input.labelRight [] <| text "px"
            }
        ]
    , wrappedRow [ spacing 10, width <| maximum 300 <| fill ]
        [ Input.button attrsButton { onPress = Just Save, label = text "save" }
        , Input.button attrsButton { onPress = Just Reset, label = text "reset" }
        , Input.button attrsButton { onPress = Just ToggleDataVisibility, label = text "show data" }
        , Input.button attrsButton { onPress = Just ToggleDataFormat, label = text "data format" }
        ]
    , paragraph [ width <| maximum 300 <| fill, Font.size 11 ]
        [ text <| "commmit " ++ model.initialFlags.meta.commit

        -- , text <| Iso8601.fromTime <| Time.millisToPosix (model.initialFlags.meta.posix * 1000)
        , text <| " version " ++ model.initialFlags.meta.version
        , text <|
            " built on "
                ++ (model.initialFlags.meta.datetime
                        |> String.replace "T" " at "
                        |> String.replace "+0900" " JST"
                   )
        ]
    ]


viewAllocations :
    Float
    -> List { b | color : String, id : Id, name : String }
    -> List ( Id, Ratio )
    -> List (Element msg)
viewAllocations scale objects latestAllocation =
    List.map
        (\( personId, ratio ) ->
            let
                maybeObject =
                    objects
                        |> List.filter (\prj -> prj.id == personId)
                        |> List.head
            in
            case maybeObject of
                Just object ->
                    el
                        [ height <| fillPortion (round ratio)
                        , paddingXY (round (20 * scale)) 0
                        , htmlAttribute <| Html.Attributes.style "background-color" object.color
                        , width fill
                        ]
                    <|
                        el [ centerY ] <|
                            text <|
                                String.fromFloat ratio
                                    ++ "% "
                                    ++ object.name

                Nothing ->
                    none
        )
        latestAllocation


viewPersonAllocationColumn : Dict.Dict Id LatestAllocation -> Float -> Model -> Person -> List (Element Msg)
viewPersonAllocationColumn latestAllocationsByPersonId scale model person =
    let
        latestAllocation : LatestAllocation
        latestAllocation =
            Maybe.withDefault latestAllocationsDefault (Dict.get person.id latestAllocationsByPersonId)
    in
    viewAllocations scale model.projects latestAllocation.allocation
        ++ [ el
                [ paddingEach { top = 4, right = 2, bottom = 2, left = 2 }
                , Font.size (round (12 * scale))
                ]
             <|
                text <|
                    "Updated: "
                        ++ latestAllocation.latest
           ]
        ++ [ el [ height <| px 5 ] <| text ""
           , Input.button attrsButton
                { onPress = Just <| ChangeDashboardStatus (EditingPerson person.id model.currentDate), label = text "edit" }
           ]


viewProjectAllocationColumn : Dict.Dict ProjectId (List ( PersonId, Ratio )) -> Float -> Model -> Project -> List (Element Msg)
viewProjectAllocationColumn latestAllocationsByProjectId scale model project =
    let
        latestAllocation : List ( PersonId, Ratio )
        latestAllocation =
            Maybe.withDefault [] (Dict.get project.id latestAllocationsByProjectId)
    in
    viewAllocations scale model.people latestAllocation
        ++ [ el [ height <| px 5 ] <| text ""
           , Input.button attrsButton
                { onPress = Just <| ChangeDashboardStatus (EditingProject project.id model.currentDate), label = text "edit" }
           ]


viewObject :
    Dict.Dict Id LatestAllocation
    -> Dict.Dict ProjectId (List ( PersonId, Ratio ))
    -> Float
    -> Object
    -> Model
    -> List (Element Msg)
viewObject latestAllocationsByPersonId latestAllocationsByProjectId scale object model =
    let
        common : Common {}
        common =
            objectToCommon object
    in
    [ column
        [ height fill
        , Font.size (round (16 * scale))
        ]
        ([ el [ paddingEach { top = 2, right = 2, bottom = 4, left = 2 } ] <| text common.name ]
            ++ (case object of
                    Person person ->
                        viewPersonAllocationColumn latestAllocationsByPersonId scale model person

                    Project project ->
                        viewProjectAllocationColumn latestAllocationsByProjectId scale model project
               )
        )
    , viewIcon scale object
    ]


viewIcon : Float -> Object -> Element msg
viewIcon scale object =
    let
        ( v1, v2, v3 ) =
            case object of
                Person _ ->
                    ( 72, 150, 79 )

                Project _ ->
                    ( 95, 200, 105 )
    in
    el
        [ width <| px (round (v1 * scale))

        -- , explain Debug.todo
        , clip
        ]
    <|
        el
            [ width <| px (round (v2 * scale))
            , moveLeft (v3 * scale)
            ]
        <|
            html <|
                case object of
                    Person _ ->
                        Svg.svg
                            [ Svg.Attributes.viewBox "95 0 322 512"
                            , Svg.Attributes.fill "rgba(100,100,100,0.3)"
                            ]
                            [ Svg.g []
                                [ Svg.path
                                    [ Svg.Attributes.d "M255.95,0c26.51,0,48,21.49,48,48s-21.49,48-48,48s-48-21.49-48-48S229.44,0,255.95,0z M263.95,352V128h6.9\n\n\t\tc33.7,0,64.9,17.7,82.3,46.6l58.3,97c9.1,15.1,4.2,34.8-10.9,43.9s-34.8,4.2-43.9-10.9l-28.7-47.7V480c0,17.7-14.3,32-32,32\n\n\t\tc-17.7,0-32-14.3-32-32V352z M154.15,182.3c19.9-33.1,55.3-53.5,93.8-54.3v256v96c0,17.7-14.3,32-32,32s-32-14.3-32-32v-96h-17.8\n\n\t\tc-10.9,0-18.6-10.7-15.2-21.1l38.3-114.8l-33.9,56.4c-9.1,15.1-28.8,20-43.9,10.9s-20-28.8-10.9-43.9L154.15,182.3z"
                                    ]
                                    []
                                ]
                            ]

                    Project _ ->
                        Svg.svg
                            [ Svg.Attributes.viewBox "0 0 95.3 95.3"
                            , Svg.Attributes.fill "rgba(100,100,100,0.3)"
                            ]
                            [ Svg.path
                                [ Svg.Attributes.d "m56.5 60.1-4.6-1c0-3.1-.8-6.2-2-9l3.6-3c.8-.6 1-1.9.3-2.8l-4.5-5.5a2 2 0 0 0-2.9-.3L43 41.3c-2.5-1.9-5.3-3.2-8.3-4v-4.5a2 2 0 0 0-2-2h-7.2a2 2 0 0 0-2 2v4.5c-3 .8-5.9 2.2-8.4 4.1l-3.5-2.8a2 2 0 0 0-2.9.3l-4.4 5.7a2 2 0 0 0 .3 2.8L8 50.3a22.7 22.7 0 0 0-1.9 9l-4.7 1a2 2 0 0 0-1.4 2.5l1.6 7A2 2 0 0 0 4 71.3L8.9 70c1.4 2.7 3.3 5 5.7 7l-2.2 4.5a2 2 0 0 0 1 2.7l6.4 3.1a2 2 0 0 0 2.7-1l2.2-4.6c3 .6 6 .6 8.9 0l2.2 4.6a2 2 0 0 0 2.7 1l6.4-3.2a2 2 0 0 0 1-2.7L43.7 77c2.3-2 4.2-4.3 5.6-7l4.8 1a2 2 0 0 0 2.4-1.5l1.5-7a2 2 0 0 0-1.5-2.4zM38 59.4a8.8 8.8 0 1 1-17.7 0 8.8 8.8 0 0 1 17.7 0zM62 43a2 2 0 0 0 1.7-1.1l1.1-2.2c1.8.3 3.7.3 5.6 0l1 2.1a2 2 0 0 0 2.7 1l2.7-1.4a2 2 0 0 0 1-2.6l-1.1-2.2c1.4-1.2 2.6-2.7 3.5-4.4l2.3.5a2 2 0 0 0 2.3-1.5l.7-3A2 2 0 0 0 84 26l-2.2-.5c0-2-.5-3.8-1.3-5.6l1.7-1.4a2 2 0 0 0 .3-2.8l-1.9-2.3a2 2 0 0 0-2.8-.3l-1.6 1.3c-1.6-1.2-3.3-2-5.2-2.5v-2a2 2 0 0 0-2-2h-3a2 2 0 0 0-2 2v2c-2 .5-3.7 1.3-5.3 2.6L57.1 13a2 2 0 0 0-2.8.3l-1.9 2.3a2 2 0 0 0 .4 2.9l1.6 1.3c-.7 1.8-1.1 3.7-1.2 5.6l-2.1.5a2 2 0 0 0-1.5 2.4l.6 3a2 2 0 0 0 2.4 1.5l2.3-.6c.9 1.7 2.1 3.2 3.6 4.4l-1 2.1a2 2 0 0 0 .9 2.7l2.7 1.3.8.2zm0-17.4a5.5 5.5 0 1 1 11 0 5.5 5.5 0 0 1-11 0zM93.8 64.1l-2.2-.5c0-2-.4-3.8-1.3-5.6l1.7-1.4a2 2 0 0 0 .3-2.8l-1.9-2.3a2 2 0 0 0-2.8-.3L86 52.5c-1.5-1.2-3.3-2-5.2-2.5v-2a2 2 0 0 0-2-2h-3a2 2 0 0 0-2 2v2a14 14 0 0 0-5.3 2.6L67 51.3a2 2 0 0 0-2.8.3L62.2 54a2 2 0 0 0 .4 2.8l1.6 1.3c-.7 1.8-1.1 3.7-1.2 5.7l-2.1.5a2 2 0 0 0-1.5 2.4l.6 2.9a2 2 0 0 0 2.4 1.5l2.3-.5c1 1.6 2.1 3 3.6 4.3l-1 2.2a2 2 0 0 0 .9 2.6l2.7 1.3a2 2 0 0 0 2.7-1l1-2.1c1.8.3 3.7.3 5.6 0l1 2.1a2 2 0 0 0 2.7 1l2.7-1.4a2 2 0 0 0 1-2.6l-1.1-2.2c1.4-1.2 2.6-2.7 3.5-4.4l2.3.5a2 2 0 0 0 2.3-1.5l.7-2.9a2 2 0 0 0-1.5-2.4zm-11-.3a5.5 5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0z"
                                ]
                                []
                            ]


personIdToPerson : Id -> List Person -> Maybe Person
personIdToPerson personId people =
    people
        |> List.filter (\p -> p.id == personId)
        |> List.head


objectToCommon : Object -> Common {}
objectToCommon object =
    case object of
        Person obj ->
            { id = obj.id
            , name = obj.name
            , hidden = obj.hidden
            , x = obj.x
            , y = obj.y
            , color = obj.color
            }

        Project obj ->
            { id = obj.id
            , name = obj.name
            , hidden = obj.hidden
            , x = obj.x
            , y = obj.y
            , color = obj.color
            }


getLatestAllocation : DateAsString -> List Allocation -> LatestAllocation
getLatestAllocation currentDate allocations =
    List.foldl
        (\( year_month_date, allocation ) acc ->
            if year_month_date > acc.latest && year_month_date <= currentDate then
                { latest = year_month_date, allocation = allocation }

            else
                acc
        )
        latestAllocationsDefault
        allocations


latestAllocationsDefault : LatestAllocation
latestAllocationsDefault =
    { latest = "0000-00-00", allocation = [] }


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
        |> Codec.field "color" .color Codec.string
        |> Codec.buildObject


codecAllocation : Codec.Codec Allocation
codecAllocation =
    Codec.tuple Codec.string <| Codec.list <| Codec.tuple Codec.string Codec.float


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
        |> Codec.field "color" .color Codec.string
        |> Codec.field "x" .x Codec.float
        |> Codec.field "y" .y Codec.float
        |> Codec.field "hidden" .hidden Codec.bool
        |> Codec.buildObject


attrsButton : List (Attribute msg)
attrsButton =
    [ Border.width 1
    , Border.rounded 20
    , Border.color <| rgba 0 0 0 0.3
    , Background.color <| rgba 1 1 1 0.7
    , paddingXY 10 3
    ]

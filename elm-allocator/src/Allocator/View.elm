module Allocator.View exposing (..)

import Allocator.Codecs
import Allocator.Components.Chart
import Allocator.Components.Icon
import Allocator.Components.Slider
import Allocator.Components.Switch
import Allocator.DataManipulation
import Allocator.Types exposing (..)
import Dict
import Diff
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events


view : Model -> Html.Html Msg
view model =
    let
        preCompiute : PreCompiute
        preCompiute =
            Allocator.DataManipulation.preCompiuteStuff_toBeCalledOnlyOncePerViewCall model
    in
    layout_new
        ([ viewDashboard model preCompiute.getPerson preCompiute.latestAllocationsByPersonId
         , Font.size 16
         , height <| px 2000
         ]
            ++ (case model.dragState of
                    Static ->
                        []

                    Moving _ ->
                        [ htmlAttribute <| Html.Attributes.style "user-select" "none" ]
               )
            ++ (case model.dashboardStatus of
                    EditingData { error } ->
                        case error of
                            Just string ->
                                [ inFront <|
                                    paragraph
                                        [ padding 20
                                        , Background.color <| rgba 1 1 1 0.9
                                        , width <| maximum 400 <| fill
                                        ]
                                        [ text string ]
                                ]

                            Nothing ->
                                []

                    _ ->
                        []
               )
        )
        (column
            ([ moveRight -model.x
             , moveDown -model.y
             , width fill
             ]
                ++ viewObjectWrapper
                    (List.map (\person -> Person person) model.people
                        ++ List.map (\project -> Project project) model.projects
                    )
                    preCompiute
                    model
            )
            []
        )


viewDashboardHeader : String -> List (Element Msg)
viewDashboardHeader title =
    [ row [ spacing 10, width fill ]
        [ paragraph [ Font.bold, paddingEach { top = 0, right = 0, bottom = 10, left = 0 } ]
            [ text title ]
        , Input.button (attrsButton ++ [ alignRight ])
            { onPress = Just <| ChangeDashboardStatus Default, label = text "close" }
        ]
    ]


viewDashboard : Model -> (PersonId -> Maybe Person) -> Dict.Dict Id LatestAllocation -> Attribute Msg
viewDashboard model getPerson latestAllocationsByPersonId =
    inFront <|
        column
            [ padding 16
            , spacing 10
            , alignRight
            , Background.color <| rgba 1 1 1 0.4
            , Font.family [ Font.monospace ]
            , Font.size 14
            , moveDown 100
            , width <| maximum 320 <| fill
            , Border.shadow { offset = ( 0, 0 ), size = 1, blur = 10, color = rgba 0 0 0 0.2 }
            , scrollbars
            , Border.roundEach { topLeft = 16, topRight = 0, bottomLeft = 16, bottomRight = 0 }

            -- , explain Debug.todo
            ]
            (case model.dashboardStatus of
                Default ->
                    viewDashboardDefault model

                EditingPerson personId dateAsString ->
                    viewDashboardHeader "Edit Person"
                        ++ viewDashboardEditingPerson
                            personId
                            dateAsString
                            model
                            getPerson
                            latestAllocationsByPersonId

                EditingProject _ _ ->
                    viewDashboardHeader "Edit Project"
                        ++ [ paragraph [] [ text "coming soon" ] ]

                EditingData args ->
                    viewDashboardHeader "Edit Raw Data"
                        ++ [ row [ alignRight, spacing 10, width fill ] [ viewJsonYamlButton model.dataFormat ]
                           , el [ height fill, width fill ] <|
                                html <|
                                    Html.textarea
                                        [ Html.Attributes.style "height" "500px"
                                        , Html.Attributes.style "border" "1px dashed #ccc"
                                        , Html.Events.onInput EditData
                                        , Html.Attributes.value args.dataAsString
                                        ]
                                        []
                           ]
            )


viewJsonYamlButton : DataFormat -> Element Msg
viewJsonYamlButton dataFormat =
    row [ spacing 5 ]
        [ Input.button
            ((case dataFormat of
                Json _ ->
                    attrsButtonSelected

                Yaml _ ->
                    attrsButton
             )
                ++ [ Border.roundEach { topLeft = 20, topRight = 0, bottomLeft = 20, bottomRight = 0 } ]
            )
            { onPress = Just ToggleDataFormat, label = text "json" }
        , Input.button
            ((case dataFormat of
                Json _ ->
                    attrsButton

                Yaml _ ->
                    attrsButtonSelected
             )
                ++ [ Border.roundEach { topLeft = 0, topRight = 20, bottomLeft = 0, bottomRight = 20 } ]
            )
            { onPress = Just ToggleDataFormat, label = text "yaml" }
        ]


viewObjectWrapper : List Object -> PreCompiute -> Model -> List (Attribute Msg)
viewObjectWrapper objects preCompiute model =
    List.map
        (\object ->
            inFront <|
                el
                    ((case model.state of
                        AllInCenter ->
                            [ moveRight 500
                            , moveDown 200
                            , alpha 0
                            , rotate 0.5
                            ]

                        OwnPosition ->
                            let
                                common : Common {}
                                common =
                                    Allocator.DataManipulation.objectToCommon object
                            in
                            [ moveRight (common.x * model.scale)
                            , moveDown (common.y * model.scale)
                            ]
                                ++ (case model.dragState of
                                        Static ->
                                            [ htmlAttribute <| Html.Attributes.style "transition" "all 600ms cubic-bezier(0.22, 1, 0.36, 1)"
                                            , alpha 0.8
                                            ]

                                        Moving args ->
                                            if .id (Allocator.DataManipulation.objectToCommon args.object) == .id (Allocator.DataManipulation.objectToCommon object) then
                                                [ alpha 1
                                                , scale 1.05
                                                ]

                                            else
                                                [ alpha 0.8 ]
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
                           , htmlAttribute <|
                                Html.Events.on
                                    "mousedown"
                                    (Allocator.Codecs.decoderForDragStart object)
                           ]
                    )
                    (viewObject preCompiute object model)
        )
        objects


viewObject : PreCompiute -> Object -> Model -> Element Msg
viewObject preCompiute object model =
    let
        common : Common {}
        common =
            Allocator.DataManipulation.objectToCommon object
    in
    column
        [ height fill
        , Font.size (round (16 * model.scale))
        ]
        (row [ spacing 10 ]
            [ el
                [ paddingEach { top = 0, right = 0, bottom = 4, left = 0 }
                , width fill
                , Font.bold
                , Font.size (round (24 * model.scale))
                ]
              <|
                text common.name
            , Input.button (attrsButton ++ [ moveUp 4 ])
                { onPress =
                    Just <|
                        ChangeDashboardStatus
                            (case object of
                                Person person ->
                                    EditingPerson person.id model.currentDate

                                Project project ->
                                    EditingProject project.id model.currentDate
                            )
                , label = text "edit"
                }
            ]
            :: (case object of
                    Person person ->
                        [ row []
                            ((if model.showCharts then
                                Allocator.Components.Chart.view model object preCompiute.projectsAsDict

                              else
                                []
                             )
                                ++ [ column [ alignTop ]
                                        (viewPersonAllocationColumn
                                            model
                                            preCompiute.latestAllocationsByPersonId
                                            person
                                        )
                                   , Allocator.Components.Icon.view model.scale object
                                   ]
                            )
                        ]

                    Project project ->
                        [ row []
                            [ column []
                                (viewProjectAllocationColumn
                                    model
                                    preCompiute.latestAllocationsByProjectId
                                    project
                                )
                            , Allocator.Components.Icon.view model.scale object
                            ]
                        ]
               )
        )


viewDashboardEditingPerson : PersonId -> DateAsString -> Model -> (PersonId -> Maybe Person) -> Dict.Dict Id LatestAllocation -> List (Element Msg)
viewDashboardEditingPerson personId dateAsString model getPerson latestAllocationsByPersonId =
    case getPerson personId of
        Nothing ->
            [ text <| "Error, " ++ personId ++ "does not exsist." ]

        Just person ->
            let
                latestAllocation : LatestAllocation
                latestAllocation =
                    Maybe.withDefault
                        Allocator.DataManipulation.latestAllocationsDefault
                        (Dict.get personId latestAllocationsByPersonId)
            in
            Input.text [ padding 3, width <| px 150 ]
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
                :: List.map
                    (\project ->
                        let
                            alloc : Float
                            alloc =
                                latestAllocation.ratiosByProjectId
                                    |> List.filter (\( projectId, _ ) -> projectId == project.id)
                                    |> List.head
                                    |> Maybe.map Tuple.second
                                    |> Maybe.withDefault 0
                        in
                        row [ spacing 10 ]
                            [ el
                                [ width <| px 20
                                , height <| px 20
                                , htmlAttribute <|
                                    Html.Attributes.style
                                        "background-color"
                                        (Allocator.DataManipulation.colorTypeToCssString project.color)
                                ]
                                none
                            , el [ width <| px 100 ] <| text <| project.name
                            , Input.text [ Font.alignRight, width <| px 40, padding 3 ]
                                { label = Input.labelRight [] (text "%")
                                , onChange =
                                    \ratio ->
                                        ChangeRatio
                                            { personId = person.id
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
                        [ Allocator.Components.Slider.view
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
                                        , x =
                                            Maybe.withDefault
                                                0
                                                (String.toFloat string)
                                        , y = person.y
                                        , hidden = person.hidden
                                        , color = person.color
                                        }
                            , placeholder = Nothing
                            , text = String.fromInt (round person.x)
                            }
                        ]
                   , row [ spacing 10 ]
                        [ Allocator.Components.Slider.view
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
                                        , y =
                                            Maybe.withDefault
                                                0
                                                (String.toFloat string)
                                        , hidden = person.hidden
                                        , color = person.color
                                        }
                            , placeholder = Nothing
                            , text = String.fromInt (round person.y)
                            }
                        ]
                   ]


viewDashboardDefault : Model -> List (Element Msg)
viewDashboardDefault model =
    let
        diffs : List (Element msg)
        diffs =
            Allocator.Codecs.defaultSavedDataformat
                |> Allocator.Codecs.encodeSavedToString model
                |> Diff.diffLines model.latestSavedAsYaml
                |> List.map
                    (\a ->
                        case a of
                            Diff.Added string ->
                                "+ " ++ string

                            Diff.NoChange string ->
                                "  " ++ string

                            Diff.Removed string ->
                                "- " ++ string
                    )
                |> addColorToDiff True
    in
    [ paragraph [ Font.size 14, Font.bold ]
        [ text <| Allocator.DataManipulation.signature1 model.flags.meta ]
    , text ""
    , Allocator.Components.Switch.view ToggleCharts model.showCharts "Toggle charts"
    , row [ spacing 10 ]
        [ Allocator.Components.Switch.view ToggleSnapToGrid model.gridSnap "Snap to grid"
        , Input.text [ padding 2, width <| px 28, Font.alignRight ]
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
    , Allocator.Components.Slider.view
        { inputFieldsWidth = 100
        , labelWidth = 50
        , label = "x"
        , max = 1000
        , min = 0
        , step = Nothing
        , value = model.x
        , msg = ChangeX
        }
    , Allocator.Components.Slider.view
        { inputFieldsWidth = 100
        , labelWidth = 50
        , label = "y"
        , max = 1000
        , min = 0
        , step = Nothing
        , value = model.y
        , msg = ChangeY
        }
    , Allocator.Components.Slider.view
        { inputFieldsWidth = 100
        , labelWidth = 50
        , label = "scale"
        , max = 5
        , min = 0.5
        , step = Nothing
        , value = model.scale
        , msg = ChangeScale
        }
    , wrappedRow [ spacing 8, width <| maximum 300 <| fill ]
        [ Input.button
            (if List.isEmpty diffs then
                attrsButton

             else
                attrsButtonSelected
            )
            { onPress = Just Save, label = text "save" }

        -- , Input.button attrsButton { onPress = Just Reset, label = text "reset" }
        , Input.button attrsButton { onPress = Just ToggleEditData, label = text "edit row data" }
        ]
    , el [ padding 5, Border.width 1, Border.color <| rgba 0 0 0 0.1, width fill ] <|
        column [ height <| px 300, scrollbars, width fill ] diffs
    , text ""
    , paragraph [ Font.size 11, Font.color <| rgba 0 0 0 0.4 ] [ text <| Allocator.DataManipulation.signature2 model.flags.meta ]
    ]


addColorToDiff : Bool -> List String -> List (Element msg)
addColorToDiff onlyDiff diffed =
    List.concatMap
        (\line ->
            if String.startsWith "+" line then
                [ el [ Font.color <| rgb 0 0.7 0 ] <| text line ]

            else if String.startsWith "-" line then
                [ el [ Font.color <| rgb 0.8 0 0 ] <| text line ]

            else if onlyDiff then
                []

            else
                [ text line ]
        )
        diffed


viewAllocations :
    Float
    -> List { b | color : ColorType, id : Id, name : String }
    -> List ( Id, Ratio )
    -> List (Element msg)
viewAllocations scale objects latestAllocation =
    -- This function is generic. It works both with People and Project.
    [ column [ height <| px (round (Allocator.Components.Chart.heightChart * scale)) ]
        (List.map
            (\( objectId, ratio ) ->
                let
                    maybeObject : Maybe { b | color : ColorType, id : Id, name : String }
                    maybeObject =
                        objects
                            |> List.filter (\prj -> prj.id == objectId)
                            |> List.head
                in
                case maybeObject of
                    Just object ->
                        el
                            [ height <| fillPortion (round ratio)
                            , paddingXY (round (20 * scale)) 0
                            , htmlAttribute <|
                                Html.Attributes.style
                                    "background-color"
                                    (Allocator.DataManipulation.colorTypeToCssString object.color)
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
        )
    ]


viewPersonAllocationColumn : Model -> Dict.Dict Id LatestAllocation -> Person -> List (Element Msg)
viewPersonAllocationColumn model latestAllocationsByPersonId person =
    let
        latestAllocation : LatestAllocation
        latestAllocation =
            Maybe.withDefault
                Allocator.DataManipulation.latestAllocationsDefault
                (Dict.get person.id latestAllocationsByPersonId)
    in
    viewAllocations model.scale model.projects latestAllocation.ratiosByProjectId
        ++ [ paragraph
                [ paddingEach { top = 4, right = 2, bottom = 2, left = 2 }
                , Font.size (round (12 * model.scale))
                , Font.alignRight
                ]
                [ text latestAllocation.latest ]
           , el [ height <| px 5 ] <| text ""
           ]


viewProjectAllocationColumn : Model -> Dict.Dict ProjectId (List ( PersonId, Ratio )) -> Project -> List (Element Msg)
viewProjectAllocationColumn model latestAllocationsByProjectId project =
    let
        latestAllocation : List ( PersonId, Ratio )
        latestAllocation =
            Maybe.withDefault [] (Dict.get project.id latestAllocationsByProjectId)
    in
    viewAllocations model.scale model.people latestAllocation


attrsButton : List (Attribute msg)
attrsButton =
    [ Border.width 1
    , Border.rounded 20
    , Border.color <| rgba 0 0 0 0.3
    , Background.color <| rgba 1 1 1 0.7
    , paddingXY 10 3
    ]


attrsButtonSelected : List (Attribute msg)
attrsButtonSelected =
    attrsButton
        ++ [ Background.color <| rgb255 18 147 216
           , Font.color <| rgb 1 1 1
           ]

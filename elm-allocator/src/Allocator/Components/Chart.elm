module Allocator.Components.Chart exposing (..)

import Allocator.DataManipulation
import Allocator.Types exposing (..)
import Chart as C
import Chart.Attributes as CA
import Date
import Dict
import Element exposing (..)
import Set


heightChart : number
heightChart =
    200


view : Model -> Object -> Dict.Dict ProjectId Project -> List (Element msg)
view model object projectsAsDict =
    case object of
        Person person ->
            let
                listData : List Data
                listData =
                    person.allocations
                        |> List.map
                            (\( dateAsString, allocations ) ->
                                dateAsString
                                    |> Date.fromIsoString
                                    |> Result.map Date.toRataDie
                                    |> Result.withDefault 0
                                    |> (\rataDie -> ( rataDie, Dict.fromList allocations ))
                            )

                projects : List ProjectId
                projects =
                    person.allocations
                        |> Allocator.DataManipulation.getProjectIdsAsSet
                        |> Set.toList

                projectIdToColor : ProjectId -> ColorType
                projectIdToColor projectId =
                    projectsAsDict
                        |> Dict.get projectId
                        |> Maybe.map .color
                        |> Maybe.withDefault ( 200, 200, 200 )
            in
            [ el
                [ alignTop
                , width <| px (round (heightChart * model.scale))
                , height <| px (round (heightChart * model.scale))
                ]
              <|
                html <|
                    C.chart
                        [ CA.height 300
                        , CA.width 300
                        ]
                        [ C.xLabels
                            [ CA.withGrid
                            , CA.rotate 30
                            , CA.fontSize 16
                            , CA.moveDown 20
                            , CA.format
                                (\num ->
                                    num
                                        |> round
                                        |> Date.fromRataDie
                                        |> Date.toIsoString
                                )
                            ]
                        , C.yLabels [ CA.withGrid ]
                        , C.series (\( dataDie, _ ) -> toFloat dataDie)
                            [ C.stacked
                                (projects
                                    |> List.map
                                        (\projectId ->
                                            C.interpolated
                                                (\( _, allocationAsDict ) ->
                                                    allocationAsDict
                                                        |> Dict.get projectId
                                                        |> Maybe.withDefault 0
                                                )
                                                [ CA.color (Allocator.DataManipulation.colorTypeToCssString (projectIdToColor projectId)), CA.opacity 0.5 ]
                                                []
                                        )
                                )
                            ]
                            listData
                        ]
            ]

        Project _ ->
            []

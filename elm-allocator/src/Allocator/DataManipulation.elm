module Allocator.DataManipulation exposing (..)

import Allocator.Types exposing (..)
import Dict
import Set


preCompiuteStuff_toBeCalledOnlyOncePerViewCall : Model -> PreCompiute
preCompiuteStuff_toBeCalledOnlyOncePerViewCall model =
    -- This function organize data in a convenient way to be used during
    -- the view execution. It is aggregate here so that it runs only
    -- once for better performance
    let
        getPerson : PersonId -> Maybe Person
        getPerson perosonId =
            Dict.get perosonId peopleAsDict

        getProject : ProjectId -> Maybe Project
        getProject projectId =
            Dict.get projectId projectsAsDict

        projectsAsDict : Dict.Dict ProjectId Project
        projectsAsDict =
            model.projects
                |> List.map (\project -> ( project.id, project ))
                |> Dict.fromList

        peopleAsDict : Dict.Dict PersonId Person
        peopleAsDict =
            model.people
                |> List.map (\person -> ( person.id, person ))
                |> Dict.fromList

        latestAllocationsByPersonId : Dict.Dict PersonId LatestAllocation
        latestAllocationsByPersonId =
            model.people
                |> List.map
                    (\person ->
                        ( person.id, getLatestAllocation model.currentDate person.allocations )
                    )
                |> Dict.fromList

        latestAllocationsByProjectId : Dict.Dict ProjectId (List ( PersonId, Ratio ))
        latestAllocationsByProjectId =
            latestAllocationsByPersonId
                |> Dict.foldl
                    (\personId { ratiosByProjectId } acc ->
                        List.foldl
                            (\( projectId, ratio ) acc2 ->
                                Dict.update projectId
                                    (\maybeV ->
                                        case maybeV of
                                            Just v ->
                                                Just <| ( personId, ratio ) :: v

                                            Nothing ->
                                                Just <| [ ( personId, ratio ) ]
                                    )
                                    acc2
                            )
                            acc
                            ratiosByProjectId
                    )
                    Dict.empty
    in
    { latestAllocationsByPersonId = latestAllocationsByPersonId
    , latestAllocationsByProjectId = latestAllocationsByProjectId
    , projectsAsDict = projectsAsDict
    , peopleAsDict = peopleAsDict
    , getProject = getProject
    , getPerson = getPerson
    }


getLatestAllocation : DateAsString -> List Allocation -> LatestAllocation
getLatestAllocation currentDate allocations =
    List.foldl
        (\( year_month_date, ratiosByProjectId ) acc ->
            if year_month_date > acc.latest && year_month_date <= currentDate then
                { latest = year_month_date, ratiosByProjectId = ratiosByProjectId }

            else
                acc
        )
        latestAllocationsDefault
        allocations


latestAllocationsDefault : LatestAllocation
latestAllocationsDefault =
    { latest = "0000-00-00", ratiosByProjectId = [] }


getProjectIdsAsSet : List ( a, List ( comparable, b ) ) -> Set.Set comparable
getProjectIdsAsSet list1 =
    List.foldl
        (\( _, list2 ) acc1 ->
            List.foldl
                (\( projectId, _ ) acc2 ->
                    Set.insert projectId acc2
                )
                acc1
                list2
        )
        Set.empty
        list1


commonTranformation : Common a -> Common {}
commonTranformation common =
    { id = common.id
    , name = common.name
    , hidden = common.hidden
    , x = common.x
    , y = common.y
    , color = common.color
    }


objectToCommon : Object -> Common {}
objectToCommon object =
    case object of
        Person obj ->
            commonTranformation obj

        Project obj ->
            commonTranformation obj


signature1 : Meta -> String
signature1 meta =
    String.join " "
        [ "Resources Allocator"
        , "POC v." ++ meta.version
        ]


signature2 : Meta -> String
signature2 meta =
    String.join " · "
        (meta.commit
            :: ("Build: "
                    ++ meta.datetime
                    |> String.replace "+0900" ""
                    |> String.split "T"
               )
        )



-- colorTypeToColor : ColorType -> Color
-- colorTypeToColor ( r, g, b ) =
--     rgb255 r g b


colorTypeToCssString : ColorType -> String
colorTypeToCssString ( r, g, b ) =
    "rgb(" ++ String.join ", " [ String.fromInt r, String.fromInt g, String.fromInt b ] ++ ")"

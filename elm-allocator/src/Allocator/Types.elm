module Allocator.Types exposing (..)

import Dict
import Http
import Time
import Url


type alias Model =
    Saved
        { x : Float
        , y : Float
        , scale : Float
        , state : State
        , showCharts : Bool
        , flags : Flags
        , posix : Time.Posix
        , currentDate : DateAsString
        , dashboardStatus : DashboardStatus
        , dataFormat : DataFormat
        , dragState : DragState
        , url : Url.Url
        , saveStatus : SaveStatus
        , gridSize : Int
        , gridSnap : Bool
        , savedCounter : Int
        , latestSavedAsYaml : String
        }


type alias Saved a =
    { a
        | people : List Person
        , projects : List Project
    }


type alias ConfluenceData =
    { pageTitle : String
    , spaceKey : String
    , pageId : String
    , spaceName : String
    , pageVersion : String
    , parentPageId : String
    , contentType : String
    , dataMacroId : String
    }


type alias Person =
    Common
        { allocations : List Allocation }


type alias Project =
    Common
        {}


type alias Common a =
    { a
        | id : Id
        , name : String
        , hidden : Bool
        , x : Float
        , y : Float
        , color : ColorType
    }


type alias ColorType =
    ( Int, Int, Int )


type alias Id =
    String


type alias Allocation =
    ( DateAsString, List ( Id, Ratio ) )


type alias DateAsString =
    String


type alias Ratio =
    Float


type State
    = AllInCenter
    | OwnPosition


type alias Flags =
    { saved : String
    , posix : Int
    , href : String
    , meta : Meta
    , maybeConflData : Maybe ConfluenceData
    }


type alias Meta =
    { commit : String
    , branch : String
    , version : String
    , datetime : String
    , posix : Int
    , tenant : String
    , service : String
    }


type DashboardStatus
    = Default
    | EditingPerson PersonId DateAsString
    | EditingProject ProjectId DateAsString
    | EditingData { dataAsString : String, error : Maybe String }


type DataFormat
    = Json Int
    | Yaml Int


type DragState
    = Static
    | Moving
        { object : Object
        , initialPersonXY : { x : Float, y : Float }
        , initialDragXY : { x : Float, y : Float }
        }


type SaveStatus
    = NotRequested
    | Saved (Result Http.Error String)
    | Saving


type Object
    = Person Person
    | Project Project


type alias LatestAllocation =
    { latest : String, ratiosByProjectId : List ( ProjectId, Ratio ) }


type Msg
    = ChangeCommonData Object (Common {})
    | ChangeRatio { personId : String, dateAsString : String, projectId : String, ratio : String }
    | ChangeX Float
    | ChangeY Float
    | ChangeScale Float
    | ChangeState State
    | ChangeGrid Int
    | Reset
    | EditData String
    | ToggleEditData
    | ToggleDataFormat
    | ToggleSnapToGrid
    | ToggleCharts
    | ChangeDashboardStatus DashboardStatus
    | DragStart Object { x : Float, y : Float }
    | DragMove Bool { x : Float, y : Float }
    | DragStop { x : Float, y : Float }
    | Save
    | SaveResponse { savedAsYaml : String } (Result Http.Error String)


type alias ProjectId =
    String


type alias PersonId =
    String


type alias Data =
    ( Int, Dict.Dict ProjectId Ratio )


type alias PreCompiute =
    { latestAllocationsByPersonId : Dict.Dict PersonId LatestAllocation
    , latestAllocationsByProjectId : Dict.Dict ProjectId (List ( PersonId, Ratio ))
    , projectsAsDict : Dict.Dict ProjectId Project
    , peopleAsDict : Dict.Dict PersonId Person
    , getProject : ProjectId -> Maybe Project
    , getPerson : PersonId -> Maybe Person
    }

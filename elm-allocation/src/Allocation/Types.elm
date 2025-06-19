module Allocation.Types exposing (..)

import Allocation.Confluence
import Http
import Time
import Url


type alias Model =
    Saved
        { x : Float
        , y : Float
        , scale : Float
        , state : State
        , showSaved : Bool
        , initialFlags : Flags
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
        }


type alias Saved a =
    { a
        | people : List Person
        , projects : List Project
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
        , color : String
    }


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
    , maybeConfluenceData : Maybe Allocation.Confluence.ConfluenceData
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
    | EditingPerson Id DateAsString
    | EditingProject Id DateAsString


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
    { latest : String, allocation : List ( Id, Ratio ) }


type Msg
    = ChangeCommonData Object (Common {})
    | ChangeRatio { personId : String, dateAsString : String, projectId : String, ratio : String }
    | ChangeX Float
    | ChangeY Float
    | ChangeScale Float
    | ChangeState State
    | ChangeGrid Int
    | Reset
    | ToggleDataVisibility
    | ToggleDataFormat
    | ToggleSnapToGrid
    | ChangeDashboardStatus DashboardStatus
    | DragStart Object { x : Float, y : Float }
    | DragMove Bool { x : Float, y : Float }
    | DragStop { x : Float, y : Float }
    | Save
    | SaveResponse (Result Http.Error String)


type alias ProjectId =
    String


type alias PersonId =
    String

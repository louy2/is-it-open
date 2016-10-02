module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Time exposing (Time, second)
import Date exposing (Date, Day(..), dayOfWeek)
import Date.Extra exposing (TimeSpec, DateSpec)
import Date.Extra as Date
import Task
import Json.Decode exposing (..)
import Result


main =
    App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { list : List Restaurant
    , date : Date
    , dataRes : String
    }


newModel =
    { list = []
    , date = Date.fromTime 0.0
    , dataRes = ""
    }


type alias Restaurant =
    { name : String
    , description : Maybe String
    , opHours : OpHours
    }


type alias OpHours =
    { mon : Maybe ( TimeSpec, TimeSpec )
    , tue : Maybe ( TimeSpec, TimeSpec )
    , wed : Maybe ( TimeSpec, TimeSpec )
    , thu : Maybe ( TimeSpec, TimeSpec )
    , fri : Maybe ( TimeSpec, TimeSpec )
    , sat : Maybe ( TimeSpec, TimeSpec )
    , sun : Maybe ( TimeSpec, TimeSpec )
    }


type alias Flags =
    { dataRes : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    { newModel | list = getData <| decodeString dataDec flags.dataRes }
        ! [ getTimeNow ]


getTimeNow : Cmd Msg
getTimeNow =
    Time.now
        |> Task.perform Tick Tick


getData : Result String (List Restaurant) -> List Restaurant
getData =
    Result.formatError (Debug.log "Parse Error") >> Result.withDefault []


dataDec : Decoder (List Restaurant)
dataDec =
    "restaurants" := list restrDec


restrDec : Decoder Restaurant
restrDec =
    object3 Restaurant
        ("name" := string)
        (maybe ("description" := string))
        ("openDuring" := opHourDec)


opHourDec : Decoder OpHours
opHourDec =
    object7 OpHours
        (maybe ("Mon" := tuple2 (,) timeSpecDec timeSpecDec))
        (maybe ("Tue" := tuple2 (,) timeSpecDec timeSpecDec))
        (maybe ("Wed" := tuple2 (,) timeSpecDec timeSpecDec))
        (maybe ("Thu" := tuple2 (,) timeSpecDec timeSpecDec))
        (maybe ("Fri" := tuple2 (,) timeSpecDec timeSpecDec))
        (maybe ("Sat" := tuple2 (,) timeSpecDec timeSpecDec))
        (maybe ("Sun" := tuple2 (,) timeSpecDec timeSpecDec))


timeSpecDec : Decoder TimeSpec
timeSpecDec =
    tuple2 timeSpecHourMinute int int


timeSpecHourMinute : Int -> Int -> TimeSpec
timeSpecHourMinute hr min =
    Date.atTime hr min 0 0



-- UPDATE


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | date = Date.fromTime newTime }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map (showRestaurant model.date) model.list)
        ]


showRestaurant : Date -> Restaurant -> Html msg
showRestaurant cdate res =
    let
        description =
            case res.description of
                Just str ->
                    p [] [ text str ]

                Nothing ->
                    text ""

        -- isOpen =
        --     showIsOpen cdate res.opHours
        -- timeRem =
        --     showTimeRem cdate res.opHours
    in
        div []
            [ h3 [] [ text res.name ]
            , description
            , showIsOpenNow cdate res.opHours
            ]


getOpSpecOnDate : Date -> OpHours -> Maybe ( TimeSpec, TimeSpec )
getOpSpecOnDate cdate oph =
    if dayOfWeek cdate == Mon then
        oph.mon
    else if dayOfWeek cdate == Tue then
        oph.tue
    else if dayOfWeek cdate == Wed then
        oph.wed
    else if dayOfWeek cdate == Thu then
        oph.thu
    else if dayOfWeek cdate == Fri then
        oph.fri
    else if dayOfWeek cdate == Sat then
        oph.sat
    else if dayOfWeek cdate == Sun then
        oph.sun
    else
        Nothing


showIsOpenNow : Date -> OpHours -> Html msg
showIsOpenNow cdate oph =
    let
        txt =
            if isOpenNow cdate oph then
                "Open"
            else
                "Close"
    in
        p [] [ text txt ]


isOpenToday : Date -> OpHours -> Bool
isOpenToday cdate oph =
    not <| getOpSpecOnDate cdate oph == Nothing


isOpenNow : Date -> OpHours -> Bool
isOpenNow cdate oph =
    let
        todaySpec =
            fromDateToDateSpec cdate
    in
        case getOpSpecOnDate cdate oph of
            Nothing ->
                False

            Just ( openTimeSpec, closeTimeSpec ) ->
                Date.isBetween
                    (Date.fromSpec Date.local openTimeSpec todaySpec)
                    (Date.fromSpec Date.local closeTimeSpec todaySpec)
                    cdate


fromDateToDateSpec : Date -> DateSpec
fromDateToDateSpec date =
    Date.calendarDate
        (Date.year date)
        (Date.month date)
        (Date.day date)

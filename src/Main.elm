import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Task
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


--MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , subTime : Bool
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) True
  , Task.perform AdjustTimeZone Time.here
  )

--UPDATE

type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | Pause
  | Resume

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )

    Pause ->
      ( { model | subTime = False }
      , Cmd.none
      )

    Resume ->
      ( { model | subTime = True }
      , Cmd.none
      )

--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.subTime then
    Time.every 1000 Tick
  else
    Sub.none

--VIEW

view : Model -> Html Msg
view model =
  let
    hour = Time.toHour model.zone model.time
    minute = Time.toMinute model.zone model.time
    second = Time.toSecond model.zone model.time
  in
    div [] [
      Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "styles.css" ] []
      , svg [ Svg.Attributes.width "250", Svg.Attributes.height "250" ] [
      circle [cx "100", cy "100", r "100", stroke "black", strokeWidth "5px", fill "none"] []
      , g [] <| List.map drawNumber <| List.range 1 12
      , drawHands second minute hour
      ]
    ]

type alias Point =
  { x : Int
  , y : Int
  }

drawHands : Int -> Int -> Int -> Svg Msg
drawHands second minute hour =
  let
    secondsHand = findPointOnCircle 60 second
    minutesHand = findPointOnCircle (60*60) (minute*60+second)
    hoursHand = findPointOnCircle (12*60) (hour*60+minute)
  in
  g [] [
  line [x1 "100", y1 "100", x2 <| String.fromInt secondsHand.x, y2 <| String.fromInt secondsHand.y
       , stroke "black", strokeWidth "3px"] []
  , line [x1 "100", y1 "100", x2 <| String.fromInt minutesHand.x, y2 <| String.fromInt minutesHand.y
       , stroke "black", strokeWidth "3px"] []
  , line [x1 "100", y1 "100", x2 <| String.fromInt hoursHand.x, y2 <| String.fromInt hoursHand.y
       , stroke "black", strokeWidth "3px"] []
  ]
drawNumber : Int -> Svg Msg
drawNumber hour =
  let
    point = findPointOnCircle 12 hour
  in
    Svg.text_ [ alignmentBaseline "middle", textAnchor "middle"
              , x <| String.fromInt point.x
              , y <| String.fromInt point.y
              ]
              [ Svg.text <| String.fromInt hour ]

findPointOnCircle : Int -> Int -> Point
findPointOnCircle slices slice =
  let
    center = Point 100 100
    r = 90
    deg = -(toFloat slice * (degrees <| 360/toFloat slices))
  in
    { x = floor (100 + r * -(sin deg)), y = floor (100 + r * -(cos deg)) }

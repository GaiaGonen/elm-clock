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
      , svg [ Svg.Attributes.width "300", Svg.Attributes.height "300" ] [
      circle [cx "150", cy "150", r "100", stroke "rgb(85, 55, 6)", strokeWidth "3px", fill "rgb(253, 227, 183)"] []
      , circle [cx "150", cy "150", r "5", fill "rgb(85, 55, 6)"] []
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
    center = Point 150 150
    secondsHand = findPointOnCircle 60 second 95 center
    secondsHandEnd = findPointOnCircle 60 second -15 center
    minutesHand = findPointOnCircle (60*60) (minute*60+second) 70 center
    minutesHandMiddleLeft = findPointOnCircle (60*60) (minute*60+second-30) 20 center
    minutesHandMiddleRight = findPointOnCircle (60*60) (minute*60+second+30) 20 center
    hoursHand = findPointOnCircle (12*60) (hour*60+minute) 60 center
    hoursHandMiddleLeft = findPointOnCircle (12*60) (hour*60+minute-20) 15 center
    hoursHandMiddleRight = findPointOnCircle (12*60) (hour*60+minute+20) 15 center
  in
  g [] [
  line [x1 <| String.fromInt secondsHandEnd.x
       , y1 <| String.fromInt secondsHandEnd.y
       , x2 <| String.fromInt secondsHand.x
       , y2 <| String.fromInt secondsHand.y
       , stroke "black", strokeWidth "1px", stroke "red"] []
  , polygon [points <| (String.fromInt center.x)++","++(String.fromInt center.y)++" "++(String.fromInt minutesHandMiddleLeft.x)++","++(String.fromInt minutesHandMiddleLeft.y)++" "++(String.fromInt minutesHand.x)++","++(String.fromInt minutesHand.y)++" "++(String.fromInt minutesHandMiddleRight.x)++","++(String.fromInt minutesHandMiddleRight.y)
       , stroke "black", strokeWidth "1px"] []
  , polygon [points <| (String.fromInt center.x)++","++(String.fromInt center.y)++" "++(String.fromInt hoursHandMiddleLeft.x)++","++(String.fromInt hoursHandMiddleLeft.y)++" "++(String.fromInt hoursHand.x)++","++(String.fromInt hoursHand.y)++" "++(String.fromInt hoursHandMiddleRight.x)++","++(String.fromInt hoursHandMiddleRight.y)
       , stroke "black", strokeWidth "1px"] []
  ]

drawNumber : Int -> Svg Msg
drawNumber hour =
  let
    point = findPointOnCircle 12 hour 85 <| Point 150 150
  in
    Svg.text_ [ alignmentBaseline "middle", textAnchor "middle"
              , x <| String.fromInt point.x
              , y <| String.fromInt point.y
              ]
              [ Svg.text <| String.fromInt hour ]

findPointOnCircle : Int -> Int -> Int -> Point -> Point
findPointOnCircle slices slice radius center =
  let
    r = toFloat radius
    deg = -(toFloat slice * (degrees <| 360/toFloat slices))
  in
    { x = floor (toFloat center.x + r * -(sin deg)), y = floor ( toFloat center.y + r * -(cos deg)) }

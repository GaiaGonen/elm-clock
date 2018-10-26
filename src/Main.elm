import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Task
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
    hour = String.fromInt (Time.toHour model.zone model.time)
    minute = String.fromInt (Time.toMinute model.zone model.time)
    second = String.fromInt (Time.toSecond model.zone model.time)
  in
    div [] [
    h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
    , if model.subTime then
      button [ onClick Pause] [ Html.text "pause" ]
    else
      button [ onClick Resume] [ Html.text "resume" ]
    ]

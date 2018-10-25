import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random
import Process
import Task
import Svg exposing (..)
import Svg.Attributes exposing (..)

--MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

--MODEL

type alias Model =
  { dieFace : Int
  , rolls : Int
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 1 0
  , Cmd.none
  )





--UPDATE

type Msg
  = Roll
  | Rolling Int
  | NewFace Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({rolls} as model) =
  case msg of
    Roll ->
      ( model
      , Random.generate Rolling (Random.int 1 6)
      )

    Rolling tmpFace ->
        if model.rolls == 6 then
          update (NewFace tmpFace) model
        else
          ( {model | rolls = rolls + 1, dieFace = tmpFace}
          , Process.sleep 100 |> Task.perform (\_ -> Roll)
          )

    NewFace newFace ->
      ( Model newFace 0
      , Cmd.none
      )


  -- Process.sleep 100 |> Task.perform (\_ -> Random.generate msg (Random.int
-- cmd : Cmd Msg
-- cmd =
--   Process.sleep 100
--       |> Task.perform (\_ -> Rolling)


--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


--VIEW

view : Model -> Html Msg
view model =
  div [] [
  drawCube model
  , drawCube model
  , button [ onClick Roll ] [ Html.text "Roll" ]
  ]

drawCube : Model -> Html Msg
drawCube model =
    svg [ Svg.Attributes.width "50", Svg.Attributes.height "50" ] [
      g [] [ rect [ x "0", y "0", Svg.Attributes.width "50", Svg.Attributes.height "50", fill "white", stroke "black", strokeWidth "5", rx "5", ry "5" ] []
      , drawDots model
      ]
    ]


drawDots : Model -> Html Msg
drawDots model =
  case model.dieFace of
    1 ->
      drawDot "25" "25"
    2 ->
      g [] [drawDot "13" "25", drawDot "37" "25"]
    3 ->
      g [] [drawDot "13" "13", drawDot "25" "25", drawDot "37" "37"]
    4 ->
      g [] [drawDot "13" "37", drawDot "37" "37", drawDot "13" "13", drawDot "37" "13"]
    5 ->
      g [] [drawDot "13" "37", drawDot "37" "37", drawDot "13" "13", drawDot "37" "13", drawDot "25" "25"]
    6 ->
      g [] [drawDot "13" "37", drawDot "37" "37", drawDot "13" "13", drawDot "37" "13", drawDot "13" "25", drawDot "37" "25"]
    _ ->
      g [] []


drawDot : String -> String -> Html Msg
drawDot x y =
  circle [cx x, cy y, r "5", fill "black"] []

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

--MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : Int
  }

init : Model
init =
  Model "" "" "" 0


-- UPDATE

type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Age Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Age age ->
      { model | age = age }

-- VIEW

view : Model -> Html Msg
view model =
  div []
  [ viewInput "text" "Name" model.name Name
  , viewInput "password" "Password" model.password Password
  , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
  , viewInput "age" "Age" (String.fromInt model.age) (String.fromInt Age)
  , viewValidation model
  ]

viewInput : String -> String -> String -> (String -> msg ) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html msg
viewValidation model =
  if passMatchValid model && passLengthValid model then
    div [ style "color" "green" ] [ text "OK!" ]
  else if passMatchValid model then
    div [ style "color" "red" ] [ text "Password must be 8 chars long!" ]
  else
    div [ style "color" "red" ] [ text "Passwords do not match!" ]

passMatchValid : Model -> Bool
passMatchValid model =
  if model.password == model.passwordAgain then
    True
  else
    False

passLengthValid : Model -> Bool
passLengthValid model =
  if String.length model.password > 8 then
    True
  else
    False

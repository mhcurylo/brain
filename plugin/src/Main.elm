module Main exposing (..)


import Html exposing (..)


main = beginnerProgram { model = model, view = view, update = update }

type alias Model =
    {
        read : List String
    }

model : Model
model =
    {
        read = ["boo"]
    }

type Msg
    = Activated String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Activated url ->
            { model | read = [url] }

view : Model -> Html Msg
view model =
  div [] [text (toString model)]

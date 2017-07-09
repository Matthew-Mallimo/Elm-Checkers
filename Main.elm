module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import List exposing (..)
import Board exposing (..)
import CheckerUtils exposing (..)
main =
    Html.beginnerProgram
    { model = initModel
    , view = view
    , update = update
    }



init : (Model, Cmd Msg)
init =
  (initModel, Cmd.none)


initModel : Model
initModel = 
    Model initialGrid True

{-----------------
      View
------------------}

view : Model -> Html Msg
view model = 
  div[][button [onClick Reset][text "Reset Board"],
    renderList model.grid
  ]


{-----------------
      Update
------------------}

update : Msg -> Model -> Model
update msg model =
  case msg of
    SelectCheker cell ->
      if cell.chkColor == "white" && model.turn then
        { model | grid = selectCell cell model.grid }
      else if cell.chkColor == "black" && not model.turn then
        { model | grid = selectCell cell model.grid }
      else
        model
    MakeMove cell ->
      let
        grid = makeMoveFromSelected cell  model.grid
      in
        case grid of
          Just newGrid ->
            { model | grid = newGrid, turn = not model.turn }
          Nothing ->
            { model | grid = clearGrid  model.grid, turn = model.turn }
    Reset ->
      initModel
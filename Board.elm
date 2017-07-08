module Board exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (..)
import CheckerUtils exposing (..)


type alias Cell =
  { x : X
  , y: Y
  , hasChecker : Bool
  , selected : Bool
  , available: Bool
  }

type alias Row = List Cell

type alias Grid = List Row


initialCell : Y -> X -> Cell
initialCell y_ x_= 
    let
        x = x_
        y = y_
        hasChecker = initialCheckerCell x_ y_
        selected = False
        available = False
    in
        Cell x y hasChecker selected available

initialRow : Y -> Row
initialRow y = 
    List.map (initialCell y) 
    <| indexedMap (\y -> (+) 1) <| range -1 6

initialGrid :  Grid
initialGrid =
    List.map initialRow (indexedMap (\y -> (+) 1) <| range -1 6)


-- This is the container for the checkerboard
gridStyle : List(String,String)
gridStyle =
    [ ("display", "inline-block")
    , ("border", "1px solid black")
    , ("align","center")
    ]

-- This is the styling for each checkerboard row
rowStyle : List(String,String)
rowStyle =
    [ ("display", "flex")
    ]

-- This is the styling for each individual cell
cellStyles : List(String,String)
cellStyles =
    [ ("backgroundColor", "red")
    , ("height", "90px")
    , ("width", "90px")
    , ("border", "1px solid black")
    ]

checkerStyles : Cell -> List(String,String)
checkerStyles cell =
    let 
        color = 
            if cell.hasChecker then
                "green"
            else
                "red"
        radius = 
            if cell.hasChecker then
                "50%"
            else
                ""
        visible = 
            if cell.hasChecker then
                "visible"
            else
                "hidden"
    in
        [ ("backgroundColor", color)
        , ("height", "80px")
        , ("width", "80px")
        , ("margin-top", "5px")
        , ("margin-left", "5px")
        , ("border-radius",radius)
        , ("visible", visible)
        ]

-- This renderes the grid to the screen
-- By providing the main div, populated
-- 
renderList : Grid -> Html msg
renderList grid =
    div [style gridStyle]
    <| List.map (div [style rowStyle] << List.map cell) grid


cell : Cell -> Html msg
cell cell =
  div
    [ style cellStyles]
    [ div
      [ style (checkerStyles cell)]
      [ text ""]
    ]
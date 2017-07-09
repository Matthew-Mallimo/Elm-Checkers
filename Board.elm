module Board exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import CheckerUtils exposing (..)
import List exposing (..)


initialCell : Y -> X -> Cell
initialCell y_ x_= 
    let
        x = x_
        y = y_
        hasChecker = initialCheckerCell x_ y_
        selected = False
        available = False
        checkerColor = getInitialCheckerColor2 x_ y_
        chkColor = getInitialCheckerColor x_ y_
    in
        Cell x y hasChecker selected available checkerColor chkColor

initialRow : Y -> Row
initialRow y = 
    List.map (initialCell y) 
    <| indexedMap (\y -> (+) 1) <| range -1 6

initialGrid :  Grid
initialGrid =
    List.map initialRow (indexedMap (\y -> (+) 1) <| range -1 6)


{-----------------
    Styles
------------------}
    
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

-- This is the styling for each Checker
checkerStyles : Cell -> List(String,String)
checkerStyles cell =
    let 
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
        selected = 
            if cell.selected then
                "2px solid cyan"
            else
                "none"
    in
        [ ("backgroundColor", cell.chkColor)
        , ("border", selected)
        , ("height", "80px")
        , ("width", "80px")
        , ("margin-top", "5px")
        , ("margin-left", "5px")
        , ("border-radius",radius)
        , ("visibility", visible)
        , ("color", "cyan")
        ]

-- This renderes the grid to the screen
-- By providing the main div, populated
-- from the grid
renderList : Grid -> Html Msg
renderList grid =
    div [style gridStyle]
    <| List.map (div [style rowStyle] << List.map cell) grid


cell : Cell -> Html Msg
cell cell =
  div
    [ style cellStyles,
        onClick <| if cell.hasChecker then (SelectCheker cell)
                    else (MakeMove cell)]
    [ div
      [ style (checkerStyles cell)]
        [text ""]
    ]
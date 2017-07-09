module CheckerUtils exposing (..)
import List exposing (..)
import Maybe exposing (andThen)
import Debug exposing (log,crash)
type alias X = Int
type alias Y = Int

type alias Cell = { 
  x : X
  , y: Y
  , hasChecker : Bool
  , selected : Bool
  , available: Bool
  , checkerColor: Maybe Color
  , chkColor : String
}

type alias Row = List Cell

type alias Grid = List Row

-- True = White
-- False = Black
type alias Turn = Bool

type alias Model = {
    grid: Grid,
    turn: Turn
}

type Color = Black | White

type Msg
  = SelectCheker Cell
  | MakeMove Cell

-- This helps place checkers at the start of the game
initialCheckerCell : X -> Y -> Bool
initialCheckerCell x_ y_ = 
  if y_ <= 2 then
      if y_ % 2 == 0 then
          if x_  % 2 == 0 then
              True
          else
              False
      else
          if x_ % 2 /= 0 then
              True
          else
              False
  else if y_ >= 5 then
      if y_ % 2 == 0 then
          if x_  % 2 == 0 then
              True
          else
              False
      else
          if x_ % 2 /= 0 then
              True
          else
              False
  else
      False

getInitialCheckerColor : X -> Y -> String
getInitialCheckerColor x_ y_ = 
  if y_ <= 2 then
      "white"
  else if y_ >= 5 then
      "black"
  else
      "none"
isTopLines : X -> Y -> Bool
isTopLines x y= y < 3


isBottomLines : X -> Y -> Bool
isBottomLines x y = y > 4


getInitialCheckerColor2 : X -> Y -> Maybe Color
getInitialCheckerColor2 x y =
  if isTopLines x y then
    Just Black

  else if isBottomLines x y then
    Just White
  else
    Nothing

updateCellsIf : (Cell -> Bool) -> (Cell -> Cell) -> Grid -> Grid
updateCellsIf pred update =
  map <| map <| \cell ->
    if pred cell
      then update cell
      else cell


updateCell : (Cell -> Cell) -> X -> Y -> Grid -> Grid
updateCell fn x_ y_ =
  updateCellsIf (\c -> (c.x == x_ && c.y == y_)) fn

updateCells : (Cell -> Cell) -> Grid -> Grid
updateCells = updateCellsIf (\c -> True)


clearGrid : Grid -> Grid
clearGrid =
  updateCells <| \cell -> { cell | selected = False, available = False }



selectCell : Cell -> Grid -> Grid
selectCell c grid =
  updateCells (\cell ->
    { cell
    | selected = (cell.x == c.x && cell.y == c.y)
    , available = isValidMove c cell grid
    }) grid


getSelected : Grid -> Maybe Cell
getSelected grid =
  head <| filter .selected <| concat grid


between : Int -> Int -> Int -> Bool
between a b x =
  (a < x && x < b) || (b < x && x < a)


makeMove : Cell -> Cell -> Grid -> Maybe Grid
makeMove from to grid =
  if isValidMove from to grid
  then Just <| updateCells (\cell ->
    { cell
    | chkColor =
        if (cell.x == to.x && cell.y == to.y) then
          from.chkColor
        else if (cell.x == from.x && cell.y == from.y) then
          "none"
        else if
          between from.x to.x cell.x &&
          between from.y to.y cell.y
        then
          "none"
        else
          cell.chkColor
    , selected = False
    , available = False
    , hasChecker = 
      if (cell.x == to.x && cell.y == to.y) then
            True
      else if (cell.x == from.x && cell.y == from.y) then
        False
      else if
        between from.x to.x cell.x &&
        between from.y to.y cell.y
      then
        False
      else
        cell.hasChecker
    }) grid
  else Nothing


makeMoveFromSelected : Cell -> Grid -> Maybe Grid
makeMoveFromSelected to grid =
  let
    selected = getSelected grid
  in
    case selected of
      Just sel ->
        makeMove sel to grid
      Nothing ->
        Just grid


isTrue : Bool -> Bool
isTrue = (==) True

-- We get the cell by flattening the grid, removing all the rows up to the cell we need
-- then removing all the cells up to the one we need. This produces a list, which we
-- Then get the head from

getCellFromPos : X -> Y -> Grid -> Maybe Cell
getCellFromPos x y grid =
  concat grid |> drop ( (y) * 8) |> drop (x) |> head 

avg : List Int -> Int
avg list =
  sum list // length list


isValidMove : Cell -> Cell -> Grid -> Bool
isValidMove cellInit cellEnd grid =
  if cellInit.chkColor == "white" then
    whiteMoveValid cellInit cellEnd grid
  else
    blackMoveValid cellInit cellEnd grid

whiteMoveValid : Cell -> Cell -> Grid -> Bool
whiteMoveValid cellInit cellEnd grid = 
  if cellEnd.hasChecker == False then
    if canWhiteMove cellInit cellEnd then
      True
    else if canWhiteJump cellInit cellEnd grid then
      True
    else
      False
  else
    False

blackMoveValid : Cell -> Cell -> Grid -> Bool
blackMoveValid cellInit cellEnd grid = 
  if cellEnd.hasChecker == False then
    if canBlackMove cellInit cellEnd then
      True
    else if canBlackJump cellInit cellEnd grid then
      True
    else
      False
  else
    False

canWhiteMove: Cell -> Cell -> Bool
canWhiteMove cellInit cellEnd =
  if
    cellEnd.y - cellInit.y == 1  &&
    abs (cellEnd.x - cellInit.x) == 1
  then
    True
  else
    False

canBlackMove: Cell -> Cell -> Bool
canBlackMove cellInit cellEnd =
  if
    cellInit.y - cellEnd.y == 1 && -- Verified tile moves forward only
    abs (cellInit.x - cellEnd.x) == 1
  then
    True
  else
    False

canWhiteJump: Cell -> Cell -> Grid -> Bool
canWhiteJump  cellInit cellEnd grid = 
  if
    cellEnd.y - cellInit.y == 2 &&       -- Verified tile moves forward only
    abs (cellEnd.x - cellInit.x) == 2 && -- This checks that the checker does not move forward
    case ((getCellFromPos (avg [cellInit.x, cellEnd.x]) (avg [cellInit.y, cellEnd.y]) grid)) of
      Just cell ->
        cell.hasChecker == True &&
        cell.chkColor /= "white"
      Nothing ->
        crash("Not there!")
        False
  then
    True
  else
    False

canBlackJump: Cell -> Cell -> Grid -> Bool
canBlackJump  cellInit cellEnd grid = 
  if
    cellInit.y - cellEnd.y == 2  &&       -- Verified tile moves forward only
    abs (cellInit.x - cellEnd.x) == 2 &&  -- This checks that the checker does not move forward
    case ((getCellFromPos (avg [cellInit.x, cellEnd.x]) (avg [cellInit.y, cellEnd.y]) grid)) of
        Just cell ->
          cell.hasChecker == True &&
          cell.chkColor /= "black"
        Nothing ->
          False
  then
    True
  else
    False
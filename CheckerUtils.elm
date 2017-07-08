module CheckerUtils exposing (..)

type alias X = Int
type alias Y = Int

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
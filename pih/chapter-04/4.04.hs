(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True

(||||) :: Bool -> Bool -> Bool
True |||| _ = True
False |||| b = b

(|||||) :: Bool -> Bool -> Bool
False ||||| False = False
True ||||| True = True
True ||||| False = True
False ||||| True = True

(||||||) :: Bool -> Bool -> Bool
b |||||| c
    | b == False && c == False = False
    | otherwise = True
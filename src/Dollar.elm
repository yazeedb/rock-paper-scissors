module Dollar exposing (Dollar, add, fromCents, fromDollars, removePercentage, times, toString)

import Round


type Dollar
    = Cents Int


fromCents : Int -> Dollar
fromCents i =
    Cents i


fromDollars : Int -> Dollar
fromDollars i =
    Cents (i * 100)


toString : Dollar -> String
toString (Cents d) =
    -- This unwraps d, making it an Int again
    "$" ++ Round.round 2 (toFloat d / 100)


add : Dollar -> Dollar -> Dollar
add (Cents d1) (Cents d2) =
    Cents (d1 + d2)


times : Int -> Dollar -> Dollar
times i (Cents d) =
    Cents (i * d)



{-
   TODO: You will experience discount bugs (add/remove) coupon
   Fix this.
-}


removePercentage : Int -> Dollar -> Dollar
removePercentage percent (Cents d) =
    let
        amountToRemove =
            toFloat d * (toFloat percent / 100)

        amount =
            d - round amountToRemove
    in
    Cents amount

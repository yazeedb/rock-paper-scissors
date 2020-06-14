module Quantity exposing (Quantity(..), toString)


type Quantity
    = Ounce Int
    | Unit String
    | MultiUnit Int String


toString : Quantity -> String
toString quantity =
    case quantity of
        Unit name ->
            name

        MultiUnit amount name ->
            String.fromInt amount ++ " " ++ name

        Ounce ounces ->
            if modBy 24 ounces == 0 then
                printPounds ounces

            else
                String.fromInt ounces ++ " ounces"


printPounds : Int -> String
printPounds ounces =
    let
        pounds =
            toFloat ounces / 24
    in
    if pounds > 1 then
        String.fromFloat pounds ++ " pounds"

    else
        "pound"

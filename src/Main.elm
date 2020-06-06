module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, h4, section, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Round



-- Domain types


type Price
    = Price Float


priceToString : Price -> String
priceToString (Price p) =
    Round.floor 2 p


type Amount
    = Amount Int


amountToString : Amount -> String
amountToString (Amount a) =
    String.fromInt a


type Discount
    = Discount Float


discountToString : Discount -> String
discountToString (Discount d) =
    String.fromFloat d


type AmountDiscount
    = AmountDiscount Float


amountDiscountToString : AmountDiscount -> String
amountDiscountToString (AmountDiscount a) =
    String.fromFloat a


type alias Item =
    { name : String
    , price : Price
    , unitOfMeasure : UnitOfMeasure
    , deal : Maybe Deal
    }


type UnitOfMeasure
    = Ounce Float
    | Bag
    | Head
    | Bushel
    | Box
    | Can
    | Unit


unitOfMeasureToString : UnitOfMeasure -> String
unitOfMeasureToString u =
    case u of
        Ounce ounces ->
            String.fromFloat ounces

        Bag ->
            "bag"

        Head ->
            "head"

        Bushel ->
            "bushel"

        Box ->
            "box"

        Can ->
            "can"

        Unit ->
            "unit"


type Deal
    = PriceDeal Amount Price
    | BulkDeal Amount AmountDiscount Discount


canOfBeans : Item
canOfBeans =
    { name = "Beans"
    , price = Price 0.5
    , unitOfMeasure = Can
    , deal = Nothing
    }


headOfLettuce : Item
headOfLettuce =
    { name = "Lettuce"
    , price = Price 1
    , unitOfMeasure = Head
    , deal = Just (PriceDeal (Amount 3) (Price 1))
    }


bagOfCashews : Item
bagOfCashews =
    { name = "Cashews"
    , price = Price 2.25
    , unitOfMeasure = Bag
    , deal = Just (BulkDeal (Amount 2) (AmountDiscount 1) (Discount 50))
    }



-- Model


type alias Model =
    { items : List Item }


initialModel : Model
initialModel =
    { items = [ canOfBeans, headOfLettuce, bagOfCashews ] }


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    div []
        (List.map
            viewItem
            model.items
        )


viewItem : Item -> Html Msg
viewItem item =
    section []
        [ h1 [] [ text item.name ]
        , h3 []
            [ text
                ("$"
                    ++ priceToString item.price
                    ++ "/"
                    ++ unitOfMeasureToString item.unitOfMeasure
                )
            ]
        , case item.deal of
            Just deal ->
                viewDeal deal

            Nothing ->
                text ""
        ]


viewDeal : Deal -> Html Msg
viewDeal deal =
    let
        dealText =
            case deal of
                PriceDeal amount price ->
                    let
                        a =
                            amountToString amount

                        p =
                            priceToString price
                    in
                    "Selling " ++ a ++ " for $" ++ p ++ "!"

                BulkDeal amount amountDiscount discount ->
                    "Buy "
                        ++ amountToString amount
                        ++ ", "
                        ++ amountDiscountToString amountDiscount
                        ++ discountToString discount
                        ++ "% off!"
    in
    div []
        [ span [ class "deal-label" ] [ text "Hot deal! " ]
        , span [ class "deal-text" ] [ text dealText ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

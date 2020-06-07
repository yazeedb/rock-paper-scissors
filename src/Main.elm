module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, h4, main_, section, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Round



-- Domain types


type alias Item =
    { name : String
    , price : Float
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
    = PriceDeal
        { amount : Float
        , price : Float
        }
    | BulkDeal
        { amount : Float
        , amountDiscount : Float
        , discount : Float
        }


canOfBeans : Item
canOfBeans =
    { name = "Beans"
    , price = 0.5
    , unitOfMeasure = Can
    , deal = Nothing
    }


headOfLettuce : Item
headOfLettuce =
    { name = "Lettuce"
    , price = 1
    , unitOfMeasure = Head
    , deal = Just (PriceDeal { amount = 3, price = 1 })
    }



-- calculateItemsPrice : List Item -> Price
-- calculateItemsPrice items =
--     case List.head items of
--         Just item ->
--             case item.deal of
--                 Just deal ->
--                     case deal of
--                         PriceDeal amount price ->
--                             -- 3/$1
--                             let
--                                 itemCount =
--                                     List.length items
--                                 remainder =
--                                     remainderBy (Amount amount) itemCount
--                             in
--                             Price 1
--                         BulkDeal amount amountDiscount discount ->
--                             Price 0
--                 Nothing ->
--                     item.price
--         Nothing ->
--             Price 0
-- let remainder = List.length items %
{-
   items.length % deal.amount
   51 % 3 == 0
   50 % 3 == 2

   const totalDealPrice = (items.length / deal.amount) * deal.price

   if (remainder == 0) {
       return totalDealPrice
   } else {
       return totalDealPrice + (remainder * item.price)
   }
-}


bagOfCashews : Item
bagOfCashews =
    { name = "Cashews"
    , price = 2.25
    , unitOfMeasure = Bag
    , deal =
        Just
            (BulkDeal
                { amount = 2
                , amountDiscount = 1
                , discount = 50
                }
            )
    }



-- Model


type alias Model =
    { items : List Item
    , cart : List Item
    }


initialModel : Model
initialModel =
    { items = [ canOfBeans, headOfLettuce, bagOfCashews ]
    , cart = []
    }


type Msg
    = AddToCart Item


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddToCart item ->
            { model | cart = model.cart ++ [ item ] }


view : Model -> Html Msg
view model =
    main_ []
        [ section [] (List.map viewItem model.items)
        , section [ class "cart" ] (viewCart model.cart)
        ]


viewItem : Item -> Html Msg
viewItem item =
    section []
        [ h1 [] [ text item.name ]
        , h3 []
            [ text
                ("$"
                    ++ String.fromFloat item.price
                    ++ "/"
                    ++ unitOfMeasureToString item.unitOfMeasure
                )
            ]
        , button [ onClick (AddToCart item) ] [ text "Add" ]
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
                PriceDeal d ->
                    "Selling "
                        ++ String.fromFloat d.amount
                        ++ " for $"
                        ++ String.fromFloat d.price
                        ++ "!"

                BulkDeal d ->
                    "Buy "
                        ++ String.fromFloat d.amount
                        ++ ", "
                        ++ String.fromFloat d.amountDiscount
                        ++ String.fromFloat d.discount
                        ++ "% off!"
    in
    div []
        [ span [ class "deal-label" ] [ text "Hot deal! " ]
        , span [ class "deal-text" ] [ text dealText ]
        ]


viewCart : List Item -> List (Html Msg)
viewCart items =
    List.map (\i -> h4 [] [ text i.name ]) items


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

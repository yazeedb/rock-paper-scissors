module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, h4, main_, section, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Round



-- Domain types


type Rule
    = Normal Item
    | HasPriceDeal Item PriceDeal
    | HasBulkDeal Item BulkDeal


type alias Item =
    { name : String
    , price : Float
    , unitOfMeasure : UnitOfMeasure
    }


type alias PriceDeal =
    { amount : Float, price : Float }


type alias BulkDeal =
    { amount : Float
    , amountDiscount : Float
    , discount : Float
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



-- Model


type alias Model =
    { items : List Item
    , cart : List Item
    }


initialModel : Model
initialModel =
    { items = []
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

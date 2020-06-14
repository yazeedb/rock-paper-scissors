module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, header, li, main_, span, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Quantity as Q exposing (Quantity)
import Round



-- Domain types


type alias StoreItem =
    { name : String
    , pricingRule : PricingRule
    }


type alias CartItem =
    { item : StoreItem
    , amountInCart : Int
    }


type alias Price =
    { basePrice : Int
    , quantity : Quantity
    }


type PricingRule
    = Normal Price
    | HasDeal Deal


type alias Deal =
    { price : Price
    , buyThisMany : Int
    , getThisMany : Int
    , atThisDiscount : Int
    , dealType : DealType
    }


type DealType
    = PriceDeal
    | BulkDeal


salmon : StoreItem
salmon =
    { name = "Salmon"
    , pricingRule =
        Normal
            { basePrice = 199
            , quantity = Q.Ounce 24
            }
    }


beans : StoreItem
beans =
    { name = "Beans"
    , pricingRule =
        HasDeal
            { price =
                { basePrice = 200
                , quantity = Q.Unit "can"
                }
            , buyThisMany = 2
            , getThisMany = 1
            , atThisDiscount = 50
            , dealType = BulkDeal
            }
    }


lettuce : StoreItem
lettuce =
    { name = "Lettuce"
    , pricingRule =
        Normal
            { basePrice = 100
            , quantity = Q.Unit "each"
            }
    }


lemons : StoreItem
lemons =
    { name = "Lemons"
    , pricingRule =
        HasDeal
            { price =
                { basePrice = 50
                , quantity = Q.Unit "lemon"
                }
            , buyThisMany = 2
            , getThisMany = 1
            , atThisDiscount = 100
            , dealType = PriceDeal
            }
    }


limes : StoreItem
limes =
    { name = "Limes"
    , pricingRule =
        HasDeal
            { price =
                { basePrice = 50
                , quantity = Q.MultiUnit 2 "Limes"
                }
            , buyThisMany = 3
            , getThisMany = 10
            , atThisDiscount = 100
            , dealType = PriceDeal
            }
    }



-- Model


type alias Model =
    { items : List StoreItem
    , cart : List CartItem
    }


initialModel : Model
initialModel =
    { items = [ salmon, beans, lettuce, lemons, limes ]
    , cart = []
    }


type Msg
    = AddToCart StoreItem Int
    | Checkout


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddToCart storeItem amount ->
            let
                predicate =
                    \i -> i.item.name == storeItem.name

                itemInCart =
                    List.any predicate model.cart
            in
            if itemInCart == True then
                let
                    newCart =
                        List.map
                            (\i ->
                                if predicate i then
                                    { i | amountInCart = i.amountInCart + amount }

                                else
                                    i
                            )
                            model.cart
                in
                { model | cart = newCart }

            else
                { model
                    | cart =
                        model.cart
                            ++ [ { item = storeItem, amountInCart = 1 }
                               ]
                }

        Checkout ->
            model


view : Model -> Html Msg
view model =
    main_ []
        [ ul [ class "items" ] (List.map viewItem model.items)
        , ul [ class "cart" ] (List.map viewCartItem model.cart)
        ]


viewItem : StoreItem -> Html Msg
viewItem item =
    let
        title =
            header []
                [ case item.pricingRule of
                    Normal p ->
                        let
                            price =
                                Round.round 2 (toFloat p.basePrice / 100)
                        in
                        h1 []
                            [ text
                                (item.name
                                    ++ " $"
                                    ++ price
                                    ++ "/"
                                    ++ Q.toString p.quantity
                                )
                            ]

                    HasDeal d ->
                        let
                            price =
                                Round.round 2 (toFloat d.price.basePrice / 100)
                        in
                        div []
                            [ span [ class "deal-label" ] [ text "Hot deal! " ]
                            , h1 []
                                [ text
                                    (item.name
                                        ++ " $"
                                        ++ price
                                        ++ "/"
                                        ++ Q.toString d.price.quantity
                                    )
                                ]
                            ]
                ]
    in
    li []
        [ title
        , h3 [] []
        , button [ onClick (AddToCart item 1) ] [ text "Add" ]
        , case item.pricingRule of
            Normal _ ->
                span [] []

            HasDeal d ->
                viewDeal d
        ]


viewDeal : Deal -> Html Msg
viewDeal { buyThisMany, getThisMany, atThisDiscount, dealType, price } =
    let
        dealText =
            case dealType of
                PriceDeal ->
                    let
                        regularBundle =
                            buyThisMany * price.basePrice

                        discountedPrice =
                            price.basePrice - price.basePrice * (atThisDiscount // 100)

                        discountedBundle =
                            getThisMany * discountedPrice

                        totalPrice =
                            Round.round 2 (toFloat (regularBundle + discountedBundle) / 100)
                    in
                    "Selling "
                        ++ String.fromInt (buyThisMany + getThisMany)
                        ++ " for $"
                        ++ totalPrice
                        ++ "!"

                BulkDeal ->
                    "Buy "
                        ++ String.fromInt buyThisMany
                        ++ ", get "
                        ++ String.fromInt getThisMany
                        ++ " "
                        ++ String.fromInt atThisDiscount
                        ++ "% off!"
    in
    div []
        [ span [ class "deal-text" ] [ text dealText ] ]


viewCartItem : CartItem -> Html Msg
viewCartItem cartItem =
    div []
        [ span [] [ text cartItem.item.name ]
        , span [] [ text (" " ++ String.fromInt cartItem.amountInCart) ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

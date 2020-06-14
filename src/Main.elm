module Main exposing (main)

import Browser
import Dollar as D exposing (Dollar)
import Html exposing (Html, button, div, h1, h3, header, li, main_, span, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Quantity as Q exposing (Quantity)



{-
   * Need to print quantities in cart.
   * How to be less verbose (unwrapping types everywhere)?
   * How to calculate cart total?
   * How to abstract Cents and avoid impossible operations?
-}
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
    { basePrice : Dollar
    , quantity : Quantity
    }


printPrice : Price -> String
printPrice p =
    D.toString p.basePrice ++ "/" ++ Q.toString p.quantity


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
            { basePrice = D.fromCents 199
            , quantity = Q.Ounce 24
            }
    }


beans : StoreItem
beans =
    { name = "Beans"
    , pricingRule =
        HasDeal
            { price =
                { basePrice = D.fromCents 200
                , quantity = Q.Unit "can"
                }
            , buyThisMany = 2
            , getThisMany = 1
            , atThisDiscount = 50

            -- , dealType = BulkDeal
            , dealType = PriceDeal
            }
    }


lettuce : StoreItem
lettuce =
    { name = "Lettuce"
    , pricingRule =
        Normal
            { basePrice = D.fromCents 100
            , quantity = Q.Unit "each"
            }
    }


lemons : StoreItem
lemons =
    { name = "Lemons"
    , pricingRule =
        HasDeal
            { price =
                { basePrice = D.fromCents 50
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
                { basePrice = D.fromCents 50
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
                    | cart = model.cart ++ [ { item = storeItem, amountInCart = 1 } ]
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
                        h1 [] [ text (item.name ++ " " ++ printPrice p) ]

                    HasDeal d ->
                        div []
                            [ span [ class "deal-label" ] [ text "Hot deal! " ]
                            , h1 [] [ text (item.name ++ " " ++ printPrice d.price) ]
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
                            D.times buyThisMany price.basePrice

                        discountedPrice =
                            D.removePercentage atThisDiscount price.basePrice

                        -- D.times
                        -- toFloat price.basePrice - toFloat price.basePrice * (toFloat atThisDiscount / 100)
                        discountedBundle =
                            D.times getThisMany discountedPrice
                    in
                    "Selling "
                        ++ String.fromInt (buyThisMany + getThisMany)
                        ++ " for "
                        ++ D.toString (D.add regularBundle discountedBundle)
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

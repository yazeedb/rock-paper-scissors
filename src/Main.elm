module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type GameResult
    = PlayerWon
    | CpuWon
    | Tie


type Choice
    = Rock
    | Paper
    | Scissors


type alias PlayerScore =
    Int


type alias CpuScore =
    Int


type alias Score =
    ( PlayerScore, CpuScore )


type GameState
    = AwaitingPlayerChoice
    | AwaitingCpuChoice
    | GameFinished


type alias Model =
    { playerScore : PlayerScore
    , cpuScore : CpuScore
    , playerChoice : Choice
    , cpuChoice : Choice
    , gameResult : GameResult
    , gameState : GameState
    }


initialModel : Model
initialModel =
    { playerScore = 0
    , cpuScore = 0
    , playerChoice = Rock
    , cpuChoice = Rock
    , gameResult = Tie
    , gameState = AwaitingPlayerChoice
    }


type Msg
    = PlayerMadeChoice Choice
    | PickCpuChoice Choice


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerMadeChoice playerChoice ->
            ( { model
                | gameState = AwaitingCpuChoice
                , playerChoice = playerChoice
              }
            , Random.generate PickCpuChoice (Random.uniform Rock [ Paper, Scissors ])
            )

        PickCpuChoice cpuChoice ->
            let
                result =
                    getGameResult cpuChoice Rock

                ( newPlayerScore, newCpuScore ) =
                    gameResultToScore result ( model.playerScore, model.cpuScore )
            in
            ( { model
                | gameState = GameFinished
                , playerScore = newPlayerScore
                , cpuScore = newCpuScore
                , cpuChoice = cpuChoice
                , gameResult = result
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    let
        titleContent =
            case model.gameState of
                AwaitingPlayerChoice ->
                    "Let the games begin!"

                AwaitingCpuChoice ->
                    "CPU Deciding..."

                GameFinished ->
                    case model.gameResult of
                        PlayerWon ->
                            "You win!"

                        CpuWon ->
                            "CPU wins!"

                        Tie ->
                            "It's a tie!"
    in
    div []
        [ header []
            [ span [ class "score-label player" ] [ text "player" ]
            , span [ class "score" ] [ text (String.fromInt model.playerScore ++ ":") ]
            , span [ class "score" ] [ text (String.fromInt model.cpuScore) ]
            , span [ class "score-label cpu" ] [ text "cpu" ]
            ]
        , div []
            [ h1 [] [ text titleContent ]
            , button [ onClick (PlayerMadeChoice Rock), class "rock-button" ] []
            , button [ onClick (PlayerMadeChoice Paper), class "paper-button" ] []
            , button [ onClick (PlayerMadeChoice Scissors), class "scissors-button" ] []
            ]
        , footer []
            [ h4 [] [ text "Make your move" ]
            ]
        ]


getGameResult : Choice -> Choice -> GameResult
getGameResult playerChoice cpuChoice =
    if playerChoice == cpuChoice then
        Tie

    else
        case playerChoice of
            Paper ->
                if cpuChoice == Rock then
                    PlayerWon

                else
                    CpuWon

            Rock ->
                if cpuChoice == Scissors then
                    PlayerWon

                else
                    CpuWon

            Scissors ->
                if cpuChoice == Paper then
                    PlayerWon

                else
                    CpuWon


gameResultToScore : GameResult -> Score -> Score
gameResultToScore result ( playerScore, cpuScore ) =
    case result of
        PlayerWon ->
            ( playerScore + 1, cpuScore )

        CpuWon ->
            ( playerScore, cpuScore + 1 )

        Tie ->
            ( playerScore + 1, cpuScore + 1 )

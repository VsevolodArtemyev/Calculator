module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String exposing (..)
import List exposing (..)
import Debug exposing (..)


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "", Cmd.none )



-- UPDATE


type Msg
    = Input String
    | Result


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input val ->
            ( model ++ val, Cmd.none )

        Result ->
            ( calcRes model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Operation
    = Plus
    | Minus
    | Mult
    | Div
    | Unknown


calcRes : Model -> String
calcRes input =
    let
        parts =
            String.split " " input

        log =
            Debug.log "" input

        a =
            List.head parts
                |> maybeStringToInt

        b =
            List.reverse parts
                |> List.head
                |> maybeStringToInt

        op =
            List.drop 1 parts
                |> List.head
                |> Maybe.withDefault "Unknown"
                |> stringToOperation
    in
        case op of
            Plus ->
                toString (a + b)

            Minus ->
                toString (a - b)

            Mult ->
                toString (a * b)

            Div ->
                toString (a // b)

            Unknown ->
                "Err"


maybeStringToInt : Maybe String -> Int
maybeStringToInt value =
    Maybe.withDefault "0" value
        |> String.toInt
        |> Result.toMaybe
        |> Maybe.withDefault 0


stringToOperation : String -> Operation
stringToOperation op =
    if op == "+" then
        Plus
    else if op == "-" then
        Minus
    else if op == "*" then
        Mult
    else if op == "/" then
        Div
    else
        Unknown



-- VIEW


view : Model -> Html Msg
view model =
    table []
        [ tr []
            [ td [ colspan 5 ]
                [ input [ onInput Input ] []
                ]
            ]
        , tr []
            [ td [] [ button [ onClick (Input "1") ] [ text "1" ] ]
            , td [] [ button [ onClick (Input "2") ] [ text "2" ] ]
            , td [] [ button [ onClick (Input "3") ] [ text "3" ] ]
            , td [] [ button [ onClick (Input "+") ] [ text "+" ] ]
            , td [] [ button [ onClick (Input "-") ] [ text "-" ] ]
            ]
        , tr []
            [ td [] [ button [ onClick (Input "4") ] [ text "4" ] ]
            , td [] [ button [ onClick (Input "5") ] [ text "5" ] ]
            , td [] [ button [ onClick (Input "6") ] [ text "6" ] ]
            , td [] [ button [ onClick (Input "*") ] [ text "*" ] ]
            , td [] [ button [ onClick (Input "/") ] [ text "/" ] ]
            ]
        , tr []
            [ td [] [ button [ onClick (Input "7") ] [ text "7" ] ]
            , td [] [ button [ onClick (Input "8") ] [ text "8" ] ]
            , td [] [ button [ onClick (Input "9") ] [ text "9" ] ]
            , td [ colspan 2 ] [ button [ onClick Result ] [ text "=" ] ]
            ]
        ]

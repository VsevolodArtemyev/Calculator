import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String
import Result exposing (..)
import List
import Debug exposing (..)

(?) : Bool -> a -> a -> a
(?) cond ifTrue ifFalse =
  if cond then ifTrue else ifFalse

main : Program Never Model Msg
main = program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- MODEL
type Operation
  = Add
  | Sub
  | Mul
  | Div
  | Eval
  | Dot
  | Clear

type alias Model =
  { display : String
  , isArithmeticLast : Bool
  , isUnaryMinusLast : Bool
  , canAddDot : Bool
  }

init : (Model, Cmd Msg)
init = (Model "0" False False False, Cmd.none)

-- UPDATE
getOperator : String -> (Float -> Float -> Float)
getOperator op =
  case op of
    "+" -> (+)
    "-" -> (-)
    "×" -> (*)
    _ -> (/)

isLastNumberZero : String -> Bool
isLastNumberZero expression =
  (String.right 2 expression == " 0") || (String.right 1 expression == "0" && String.length expression == 1)

addDigit : Float -> Model -> Model
addDigit value model =
  { model
  | display = model.display ++ (toString value)
  , isArithmeticLast = False
  , isUnaryMinusLast = False
  }

replaceZeroByDigit : Float -> Model -> Model
replaceZeroByDigit value model =
  { model | display = String.dropRight 1 model.display }
  |> addDigit value


evaluateExpression : Model -> Model
evaluateExpression model =
  let
    isOperator = (\op -> List.member op ["+", "-", "÷", "×", "_"])

    shapePolishNotation op ac =
      let
        isHigherOrEqual = (\order -> List.member order [GT, EQ])
        getLastOperator = (\stack -> List.head stack |> Maybe.withDefault "!")

        getOperationRank = (\op ->
          if List.member op ["+", "-"] then
            1
          else if List.member op ["×", "÷"] then
            2
          else
            0
        )

        isPopPreviosOperator = (\stackOp
          -> isHigherOrEqual
          <| compare (getOperationRank stackOp)
          <| getOperationRank op
        )

      in
        if not (isOperator op) then
          { ac | operands = op :: ac.operands }
        else
          let
            pulledOperators = (List.filter isPopPreviosOperator ac.operators) |> List.reverse
            newOperatorsStack = ac.operators |> List.drop (List.length pulledOperators)

          in
            { ac
            | operands = List.append pulledOperators ac.operands
            , operators = op :: newOperatorsStack
            }


    listToTuple list =
      let
        a = List.head list |> Result.fromMaybe "Error" |> Result.andThen String.toFloat
        b = List.reverse list |> List.head |> Result.fromMaybe "Error" |> Result.andThen String.toFloat

      in
        (a, b)

    executePolishNotation op stack =
      if not (isOperator op) then
        op :: stack
      else
        let
          st = log "stack: " stack

          (op2, op1) = List.take 2 stack |> listToTuple

          deb = log "op1" op1
          deb2 = log "op2" op2

          operator = getOperator op

          result = 
            case (Result.map2 operator op1 op2) of
              Ok value -> toString value
              Err err -> "Error"

        in
          result :: (List.drop 2 stack)

    result =
      (model.display ++ " _")
      |> String.split " "
      |> List.foldl shapePolishNotation {operands = [], operators = []}
      |> .operands
      |> List.foldr executePolishNotation []
      |> List.head
      |> Maybe.withDefault "Error"

  in
    if result == "Error" then
      (Model result (List.singleton [Clear]) False)
    else
      (Model result (List.singleton [Dot, Clear, Add, Sub, Mul, Div]) False)


type Msg
  = Press Float
  | Execute Operation

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Press value ->
      if isLastNumberZero model.display then
        (replaceZeroByDigit value model, Cmd.none)
      else
        (addDigit value model, Cmd.none)

    Execute oper ->
      case oper of
        Add ->
          if model.isArithmeticLast then
            { model | }

        Sub -> 
          let
            updateLegitMatrixBy = model.isArithmeticLast ?
              updateLegitMatrix [Dot] [Add, Sub, Mul, Div, Eval] <|
              updateLegitMatrix [Dot, Sub] [Add, Mul, Div, Eval]

            newValue = model.isArithmeticLast ? "-" <| " - "

          in (
            Model
              (model.display ++ newValue)
              (updateLegitMatrixBy model.legitOperationsMatrix)
              True

            , Cmd.none
          )

        Mul -> (
          Model
            (model.display ++ " × ")
            (updateLegitMatrix [Dot, Sub] [Add, Mul, Div, Eval] model.legitOperationsMatrix)
            True

          , Cmd.none
        )

        Div -> (
          Model
            (model.display ++ " ÷ ")
            (updateLegitMatrix [Dot, Sub] [Add, Mul, Div, Eval] model.legitOperationsMatrix)
            True

          , Cmd.none
        )

        Dot -> (
          Model
            (model.display ++ (model.isArithmeticLast ? "0." <| "."))
            (updateLegitMatrix [] [Add, Sub, Mul, Div, Eval, Dot] model.legitOperationsMatrix)
            False

          , Cmd.none
        )

        Eval ->
          (evaluateExpression model, Cmd.none)

        Clear ->
          case String.right 1 model.display of
            " " -> (
              Model
                (String.dropRight 3 model.display)
                (List.drop 1 model.legitOperationsMatrix)
                False

              , Cmd.none
            )

            _ -> 
              let
                newDisplay = String.dropRight 1 model.display

              in (
                Model
                  newDisplay
                  (List.drop 1 model.legitOperationsMatrix)
                  (String.right 1 newDisplay == " ")

                , Cmd.none
              )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
    
-- VIEW

stylesheet : String -> Html a
stylesheet href =
  let
    tag = "link"
    attrs =
      [ attribute "Rel" "stylesheet"
      , attribute "property" "stylesheet"
      , attribute "href" href
      ]
    child = []

  in
    node tag attrs child
      

view : Model -> Html Msg
view model =
  div [ class "calculator" ]
  [ stylesheet "styles.css"
  , div [ class "input" ] [ text model.display ]
  , div [ class "btns" ]
    [ div [ class "clear", onClick (Execute Clear) ] [ text "Clear" ]
    , span [ onClick (Press 1) ]      [ text "1" ]
    , span [ onClick (Press 2) ]      [ text "2" ]
    , span [ onClick (Press 3) ]      [ text "3" ]
    , span [ onClick (Execute Add) ]  [ text "+" ]
    , span [ onClick (Press 4) ]      [ text "4" ]
    , span [ onClick (Press 5) ]      [ text "5" ]
    , span [ onClick (Press 6) ]      [ text "6" ]
    , span [ onClick (Execute Sub) ]  [ text "-" ]
    , span [ onClick (Press 7) ]      [ text "7" ]
    , span [ onClick (Press 8) ]      [ text "8" ]
    , span [ onClick (Press 9) ]      [ text "9" ]
    , span [ onClick (Execute Mul) ]  [ text "×" ]
    , span [ onClick (Press 0) ]      [ text "0" ]
    , span [ onClick (Execute Dot) ]  [ text "." ]
    , span [ onClick (Execute Eval) ] [ text "=" ]
    , span [ onClick (Execute Div) ]  [ text "÷" ]
    ]
  ]
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String
import List
--import Debug exposing (..)

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
  | Zero
  | Dot
  | Clear

type alias Model =
  { display : String
  , legitOperationsList : List Operation
  , canAddDigit : Bool
  }

init : (Model, Cmd Msg)
init = (Model "" [Dot, Zero, Clear] True, Cmd.none)

-- UPDATE

shapeLegitList : List Operation -> List Operation -> List Operation
shapeLegitList base additional =
  let
    filtered = List.filter (\op -> not (List.member op additional)) base
      
  in
    List.append additional filtered

isLastNumberZero : String -> Bool
isLastNumberZero expression =
  String.right 2 expression == " 0"

replaceZeroByDigit : Model -> Float -> Model
replaceZeroByDigit model value =
  let
    baseExpression = String.dropRight 2 model.display
    newValue = value |> toString |> String.padLeft 2 ' '

  in
    { model
    | display = baseExpression ++ newValue
    , legitOperationsList = shapeLegitList model.legitOperationsList [Add, Sub, Mul, Div]
    , canAddDigit = True
    }

addDigit : Model -> Float -> Model
addDigit model value =
  { model
  | display = model.display ++ (toString value)
  , legitOperationsList = shapeLegitList model.legitOperationsList [Add, Sub, Mul, Div]
  , canAddDigit = True
  }

type Msg
  = Press Float
  | Execute Operation

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Press value ->
      if not model.canAddDigit then
        (model, Cmd.none)
      else if isLastNumberZero model.display then
        (replaceZeroByDigit model value, Cmd.none)
      else
        (addDigit model value, Cmd.none)

    Execute oper ->
      if List.member oper model.legitOperationsList then
        case oper of
          Add ->
            (model, Cmd.none)
          Sub ->
            (model, Cmd.none)
          Mul ->
            (model, Cmd.none)
          Div ->
            (model, Cmd.none)
          Zero ->
            (model, Cmd.none)
          Dot ->
            (model, Cmd.none)
          Eval ->
            (model, Cmd.none)
          Clear ->
            ({model | display = model.display |> String.dropRight 1}, Cmd.none)
      else
        (model, Cmd.none)
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
    , span [ onClick (Press 1) ] [ text "1" ]
    , span [ onClick (Press 2) ] [ text "2" ]
    , span [ onClick (Press 3) ] [ text "3" ]
    , span [ onClick (Execute Add) ] [ text "+" ]
    , span [ onClick (Press 4) ] [ text "4" ]
    , span [ onClick (Press 5) ] [ text "5" ]
    , span [ onClick (Press 6) ] [ text "6" ]
    , span [ onClick (Execute Sub) ] [ text "-" ]
    , span [ onClick (Press 7) ] [ text "7" ]
    , span [ onClick (Press 8) ] [ text "8" ]
    , span [ onClick (Press 9) ] [ text "9" ]
    , span [ onClick (Execute Mul) ] [ text "x" ]
    , span [ onClick (Execute Zero) ] [ text "0" ]
    , span [ onClick (Execute Dot) ] [ text "." ]
    , span [ onClick (Execute Eval) ] [ text "=" ]
    , span [ onClick (Execute Div) ] [ text "÷" ]
    ]
  ]
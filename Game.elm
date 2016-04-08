module Game (..) where


{-|
# Game of logic

Copyright Théo Zimmermann 2016. License MPL 2.0

@docs main
-}


import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Array exposing (Array)
import StartApp.Simple as StartApp


{- # MAIN -}


{-| Run the game. -}
main : Signal Html
main =
  StartApp.start { model = init 0, view = view, update = update }


{- # UTILS -}


singleton : x -> List x
singleton x = [x]


{- ## Custom events -}


messageOn : String -> Signal.Address a -> a -> Attribute
messageOn name addr msg =
  on name Json.value (\_ -> Signal.message addr msg)


onDragStart = messageOn "dragstart"


onDragOver addr msg =
  onWithOptions
    "dragover"
    { preventDefault = True, stopPropagation = False}
    Json.value
    (\_ -> Signal.message addr msg)


onDrop = messageOn "drop"


{- # MODEL -}


-- Propositional calculus to start with


type Formula
  = Var String
  | Impl Formula Formula


levelMax = 1


levels : Int -> List Formula
levels index =
  let
    a = Var "A"
    b = Var "B"
    c = Var "C"
    d = Var "D"
  in
  if index == 0 then
    [ Impl a b
    , Impl a (Impl c d)
    , a
    , Impl b c
    ]

  else
    [ Impl a b
    , Impl c b
    , Impl c d
    , b
    , Impl (Impl a b) (Impl b c)
    ]


type alias Model =
  { context : Array Formula
  , selected : Maybe Formula
  , message : String
  , finished : Bool
  }


init : Int -> Model
init level =
  { context =
      Array.fromList <| levels level
  , selected = Nothing
  , message =
      if level == 0 then "Try moving A on A ⇒ B." else ""
  , finished = False
  }


{- # VIEW -}


addPar : Formula -> String
addPar f =
  case f of
    Var s ->
      s

    _ ->
      "(" ++ formulaToString f ++ ")"

formulaToString : Formula -> String
formulaToString f =
  case f of
    Var s ->
      s

    Impl f1 f2 ->
       addPar f1 ++ " ⇒ " ++ addPar f2

smallMargin = ("margin" , "4px 0")
smallPadding = ("padding" , "2px")
grayBackground = ("background-color" , "gray")
redBorder = ("border" , "2px solid red")
fixedHeight = ("height" , "30px")
italic = ("font-style" , "italic")


viewFormula : Signal.Address Action -> Int -> Formula -> Html
viewFormula address index formula =
  div
    [ style
        [ smallMargin
        , smallPadding
        , fixedHeight
        ]
    ]
    [ span
      [ draggable "true"
      , onDragStart address <| DragStart (index , formula)
      , onDragOver address NoOp
      , onDrop address <| Drop (index , formula)
      , style
          [ grayBackground
          , redBorder
          , smallPadding
          ]
      ]
      [ text <| formulaToString formula ]
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [("margin" , "10px")]
    ]
    [ div [] (Array.toList <| Array.indexedMap (viewFormula address) model.context)
    , div [ style  [ smallMargin , fixedHeight ] ] [ text "The goal is to get D." ]
    , div
        [ style  [ fixedHeight , italic ] ]
        [ text <| model.message ]
    ]


{- # ACTIONS AND UPDATE -}


type Action
  = NoOp
  | DragStart (Int, Formula)
  | Drop (Int, Formula)


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    DragStart (index, formula) ->
      { model | selected = Just formula }

    Drop (index, formula) ->
      let
        updatedFormula =
          case (formula , model.selected) of
            (Impl f1 f2 , Just f3) ->
              if f1 == f3 then
                f2
              else
                formula

            _ -> formula

        -- if the game was already finished it stays finished
        finished = model.finished || updatedFormula == Var "D"
      in
          { context = Array.set index updatedFormula model.context
          , selected = Nothing
          , message =
              if finished then "You win!" else ""
          , finished = finished
          }


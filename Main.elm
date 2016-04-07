module Main (main) where


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
  StartApp.start { model = initModel, view = view, update = update }


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


type alias Model =
  { context : Array Formula
  , selected : Maybe Formula
  , message : String
  }


initModel : Model
initModel =
  { context =
    [ Impl (Var "A") (Var "B")
    , Impl (Var "A") (Impl (Var "C") (Var "D"))
    , Var "A"
    , Impl (Var "B") (Var "C")
    ] |> Array.fromList
  , selected = Nothing
  , message = "Try moving A on A ⇒ B."
  }


{- # VIEW -}


addPar f =
  case f of
    Var s ->
      s

    _ ->
      "(" ++ formulaToString f ++ ")"


formulaToString f =
  case f of
    Var s ->
      s

    Impl f1 f2 ->
       addPar f1 ++ " ⇒ " ++ addPar f2

smallMargin = ("margin" , "4px 0")
bigMargin = ("margin" , "10px 0")
smallPadding = ("padding" , "2px")
grayBackground = ("background-color" , "gray")
redBorder = ("border" , "2px solid red")
boxHeight = ("height" , "30px")
italic = ("font-style" , "italic")


viewFormula : Signal.Address Action -> Int -> Formula -> Html
viewFormula address index formula =
  div
    [ style
        [ smallMargin
        , smallPadding
        , boxHeight
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
    , div [ style  [ bigMargin ] ] [ text "The goal is to get D." ]
    , div
        [ style  [ bigMargin , italic ] ]
        [ text <| model.message ]
    ]


{- # ACTIONS AND UPDATE -}


type Action
  = NoOp
  | DragStart (Int, Formula)
  | Drop (Int, Formula)


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
      in
          { context = Array.set index updatedFormula model.context
          , selected = Nothing
          , message =
              if updatedFormula == Var "D" then "You win!" else ""
          }


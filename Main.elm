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


viewFormula : Signal.Address Action -> Int -> Formula -> Html
viewFormula address index formula =
  div
    [ style
        [ ("margin" , "4px 0")
        , ("padding" , "2px")
        ]
    ]
    [ span
      [ draggable "true"
      , onDragStart address <| DragStart (index , formula)
      , onDragOver address NoOp
      , onDrop address <| Drop (index , formula)
      , style
          [ ("background-color" , "gray")
          , ("border" , "2px solid red")
          , ("padding" , "2px")
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
    , div [ style  [ ("margin" , "10px 0")] ] [text "The goal is to get D." ]
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
        updatedContext =
          case (formula , model.selected) of
            (Impl f1 f2 , Just f3) ->
              if f1 == f3 then
                Array.set index f2 model.context
              else
                model.context

            _ -> model.context
      in
          { context = updatedContext
          , selected = Nothing
          }


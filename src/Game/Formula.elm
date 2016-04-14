module Game.Formula (Formula(..), toString, view, Action(..)) where


import Maybe exposing (andThen)
import Html exposing (..)
import Html.Attributes exposing (..)
import DragDrop exposing (..)
import Style exposing (..)


-- Propositional calculus to start with


{-| # Model -}

type Formula
  = Var String
  | Impl Formula Formula


-- Takes two formulas and return a resulting formula if they can be combined by modus ponens
combine : Formula -> Formula -> Maybe Formula
combine f f' =
  case (f , f') of
    (Impl f1 f2 , Impl f1' f2') ->
      if f1 == f' then
        Just f2
      else if f1' == f then
        Just f2'
      else
        Nothing

    (Impl f1 f2 , _) ->
      if f1 == f' then
        Just f2
      else
        Nothing

    (_ , Impl f1' f2') ->
      if f1' == f then
        Just f2'
      else
        Nothing

    _ -> Nothing


{-| # View -}


addPar : Formula -> String
addPar f =
  case f of
    Var s ->
      s

    _ ->
      "(" ++ toString f ++ ")"


toString : Formula -> String
toString f =
  case f of
    Var s ->
      s

    Impl f1 f2 ->
       addPar f1 ++ " ⇒ " ++ addPar f2


view : Maybe Formula -> Signal.Address Action -> Formula -> Html
view selected address formula =
  let
    combined = selected `andThen` combine formula

    (dropAction , dropEffect) =
      case combined of
        Just f ->
          (Result f , All)

        Nothing ->
          (NoOp , None)

  in

  div
    [ style
        [ smallMargin
        , smallPadding
        , fixedHeight
        ]
    ]
    [ span
      [ draggable "true"
      , onDragStart address Selected
      , onDragOver dropEffect address NoOp
      , onDrop address dropAction
      , style
          [ grayBackground
          , redBorder
          , smallPadding
          ]
      ]
      [ text <| toString formula ]
    ]


{-| # Actions -}


type Action
  = NoOp
  | Selected
  | Result Formula


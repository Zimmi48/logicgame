module Game.Context (..) where


import Array exposing (Array)
import Html exposing (..)
import Game.Formula as Formula exposing (Formula, Action(..))


{-| # Model -}


type alias Context a =
  { hypothesis : a
  , formulas : Array Formula
  }


fromList : List Formula -> Context ()
fromList formulas =
  { hypothesis = ()
  , formulas = Array.fromList formulas
  }


empty : Formula -> Context Formula
empty hypothesis =
  { hypothesis = hypothesis
  , formulas = Array.empty
  }

{-| # View -}


view : Maybe Formula -> Signal.Address Action -> Context a -> Html
view selected address context =
  div []
    ( Array.toList <|
      Array.indexedMap
        (\index formula ->
            Formula.view
              selected
              (Signal.forwardTo address <| FormulaAction index formula)
              formula
        )
        context.formulas
    )


{-| # Actions and update -}


type Action
  = NoOp
  | FormulaAction Int Formula Formula.Action


update : Action -> Context a -> Context a
update action context =
  case action of

    FormulaAction index _ (Result updatedFormula) ->
      { context |
        formulas = Array.set index updatedFormula context.formulas
      }

    _ ->
      context
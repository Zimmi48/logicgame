module Game.Context exposing (..)


import Array exposing (Array)
import Html exposing (..)
import Html.App
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


view : Maybe Formula -> Context a -> Html Action
view selected context =
  div []
    ( Array.toList <|
      Array.indexedMap
        (\index formula ->
            Html.App.map
              (FormulaAction index formula)
              (Formula.view selected formula)
        )
        context.formulas
    )


{-| # Actions and update -}


type Action
  = NoOp
  | FormulaAction Int Formula Formula.Action
  | AddFormula Formula


update : Action -> Context a -> Context a
update action context =
  case action of

    FormulaAction index _ (Result updatedFormula) ->
      { context |
        formulas = Array.set index updatedFormula context.formulas
      }

    AddFormula formula ->
      { context |
        formulas = Array.push formula context.formulas
      }

    _ ->
      context
module Game.Update (update) where


import Array
import Time
import Game.Formula as Formula exposing (Formula, Action(..))
import Game.Context as Context exposing (Action(..))
import Game.Actions exposing (..)
import Game.Model exposing (Model, hints)

addFormulaToMainContext : Formula -> Context.Action -> Model -> Model
addFormulaToMainContext formula action model =
  let
  -- if the game was already finished it stays finished
    finished = model.finished || formula == model.goal

  in

  { model |
    mainContext = Context.update action model.mainContext
  , selected = Nothing
  , selectionContext = Nothing
  , message = if finished then "You win!" else ""
  , finished = finished
  }

update : Game.Actions.Action -> Model -> Model
update action model =
  case action of
    MainContextAction (FormulaAction _ formula Selected) ->
      { model |
        selected = Just formula
      , selectionContext = Nothing
      }

    MainContextAction (FormulaAction _ _ (Result formula) as action) ->
      addFormulaToMainContext formula action model

    MainContextAction (AddFormula formula as action) ->
      addFormulaToMainContext formula action model

    ContextAction _ hypothesis (FormulaAction _ formula Selected) ->
      { model |
        selected = Just formula
      , selectionContext = Just hypothesis
      }

    ContextAction index _ action ->
      { model |
        contexts =
          Array.indexedMap
            (\i c -> if i == index then Context.update action c else c)
            model.contexts
      }

    Time time ->
      { model |
        message = hints time model
      }

    _ ->
      model

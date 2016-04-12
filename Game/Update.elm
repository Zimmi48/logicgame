module Game.Update (update) where


import Array
import Game.Formula as Formula exposing (Action(..))
import Game.Context as Context exposing (Action(..))
import Game.Actions exposing (..)
import Game.Model exposing (Model)


update : Game.Actions.Action -> Model -> Model
update action model =
  case action of
    MainContextAction (FormulaAction _ formula Selected) ->
      { model |
        selected = Just formula
      , selectionContext = Nothing
      }

    MainContextAction (FormulaAction _ _ (Result updatedFormula) as action) ->
      let
        -- if the game was already finished it stays finished
        finished = model.finished || updatedFormula == model.goal
      in
      { model |
        mainContext = Context.update action model.mainContext
      , selected = Nothing
      , selectionContext = Nothing
      , message = if finished then "You win!" else ""
      , finished = finished
      }

    MainContextAction (AddFormula formula as action) ->
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
      , message = if model.finished then model.message else ""
      }

    _ ->
      model

module Game.Update (update) where


import Array
import Game.Formula as Formula exposing (Action(..))
import Game.Actions exposing (..)
import Game.Model exposing (Model)


update : Game.Actions.Action -> Model -> Model
update action model =
  case action of
    FormulaAction _ formula Selected ->
      { model | selected = Just formula }

    FormulaAction index _ (Result updatedFormula) ->
      let
        -- if the game was already finished it stays finished
        finished = model.finished || updatedFormula == model.goal
      in
      { model |
        mainContext = Array.set index updatedFormula model.mainContext
      , selected = Nothing
      , message = if finished then "You win!" else ""
      , finished = finished
      }

    _ ->
      model

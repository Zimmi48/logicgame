module Game.Update (update) where


import Array
import Game.Actions exposing (..)
import Game.Model exposing (Model)


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    DragStart (index, formula) ->
      { model | selected = Just formula }

    Drop (index, updatedFormula) ->
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

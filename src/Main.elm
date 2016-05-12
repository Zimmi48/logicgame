
module Main exposing (main)


{-|
# Game of logic

Copyright Théo Zimmermann 2016. License MPL 2.0

-}


import Time exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Game.View
import Game.Update
import Game.Model exposing (levelMax)
import Game.Actions


{- # MAIN -}


main =
  Html.App.program
    { init = (initModel, Cmd.none)
    , view = view
    , update = \a m -> (update a m , Cmd.none)
    , subscriptions = (\_ -> every second (Game.Actions.Time >> GameAction))
    }


--port title : String
--port title = "A simple logic game"


{- # MODEL -}


type alias Model =
  { game : Game.Model.Model
  , maxUnlocked : Int
  , level : Int
  }


initModel : Model
initModel =
  { game = Game.Model.init 0
  , maxUnlocked = 0
  , level = 0
  }


init : Int -> Model -> Model
init index model =
  { model |
    game = Game.Model.init index
  , level = index
  }


{- # VIEW -}


view : Model -> Html Action
view model =
  div
    [ style [("margin" , "10px")]
    ]
    [ button [ onClick Restart ] [ text "Restart" ]
    , button
        [ hidden (model.level == 0)
        , onClick PreviousLevel
        ] [ text "Previous level" ]
    , button
        [ disabled (model.maxUnlocked <= model.level)
        , hidden (levelMax <= model.level)
        , onClick NextLevel
        ] [ text "Next level" ]
    , Html.App.map GameAction (Game.View.view model.game)
    ]


{- # ACTIONS AND UPDATE -}


type Action
  = NoOp
  | GameAction Game.Actions.Action
  | Restart
  | NextLevel
  | PreviousLevel


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    GameAction action ->
      let
        game = Game.Update.update action model.game
      in
        { model |
          game = game
        , maxUnlocked =
            if game.finished then
              Basics.max model.maxUnlocked <| model.level + 1
            else
              model.maxUnlocked
        }

    Restart ->
      init model.level model

    NextLevel ->
      init (model.level + 1) model

    PreviousLevel ->
      init (model.level - 1) model


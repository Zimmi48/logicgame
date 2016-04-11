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
import Game.View
import Game.Update
import Game.Model exposing (levelMax)
import Game.Actions


{- # MAIN -}


{-| Run the game. -}
main : Signal Html
main =
  StartApp.start { model = initModel, view = view, update = update }


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


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [("margin" , "10px")]
    ]
    [ button [ onClick address Restart ] [ text "Restart" ]
    , button
        [ hidden (model.level == 0)
        , onClick address PreviousLevel
        ] [ text "Previous level" ]
    , button
        [ disabled (model.maxUnlocked <= model.level)
        , hidden (levelMax <= model.level)
        , onClick address NextLevel
        ] [ text "Next level" ]
    , Game.View.view (Signal.forwardTo address GameAction) model.game
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


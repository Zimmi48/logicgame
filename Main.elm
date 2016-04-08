module Main (main) where


{-|
# Game of logic

Copyright Théo Zimmermann 2016. License MPL 2.0

@docs main
-}


import Game
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
  StartApp.start { model = init 0, view = view, update = update }


{- # MODEL -}


type alias Model =
  { game : Game.Model
  , nextActive : Bool
  , level : Int
  }


init : Int -> Model
init index =
  { game = Game.init index
  , nextActive = False
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
        [ disabled (not model.nextActive)
        , hidden (Game.levelMax <= model.level)
        , onClick address NextLevel
        ] [ text "Next level" ]
    , Game.view (Signal.forwardTo address GameAction) model.game
    ]


{- # ACTIONS AND UPDATE -}


type Action
  = NoOp
  | GameAction Game.Action
  | Restart
  | NextLevel


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    GameAction action ->
      let
        game = Game.update action model.game
      in
        { model |
          game = game
        , nextActive = game.finished
        }

    Restart ->
      init model.level

    NextLevel ->
      init <| model.level + 1


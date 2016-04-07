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
  StartApp.start { model = initModel, view = view, update = update }


{- # MODEL -}


type alias Model =
  { game : Game.Model
  , nextActive : Bool
  }


initModel : Model
initModel =
  { game = Game.initModel
  , nextActive = False
  }


{- # VIEW -}


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [("margin" , "10px")]
    ]
    [ Game.view (Signal.forwardTo address GameAction) model.game
    , button [] [ text "Restart" ]
    , button [ disabled (not model.nextActive) ] [ text "Next level" ]
    ]


{- # ACTIONS AND UPDATE -}


type Action
  = NoOp
  | GameAction Game.Action


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    GameAction action ->
      let
        game = Game.update action model.game
      in
        { game = game
        , nextActive = game.finished
        }


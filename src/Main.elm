
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
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> every second (Game.Actions.Time >> GameMsg))
    }


{- # MODEL -}


type alias Model =
  { game : Game.Model.Model
  , maxUnlocked : Int
  , level : Int
  }


init =
  ( { game = Game.Model.init 0
    , maxUnlocked = 0
    , level = 0
    }
  , Cmd.none )


newLevel : Int -> Model -> (Model, Cmd Msg)
newLevel index model =
  ( { model |
      game = Game.Model.init index
    , level = index
    }
  , Cmd.none )


{- # VIEW -}


view : Model -> Html Msg
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
    , Html.App.map GameMsg (Game.View.view model.game)
    ]


{- # ACTIONS AND UPDATE -}


type Msg
  = NoOp
  | GameMsg Game.Actions.Action
  | Restart
  | NextLevel
  | PreviousLevel


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    NoOp ->
      ( model , Cmd.none )

    GameMsg action ->
      let
        game = Game.Update.update action model.game
      in
        ( { model |
            game = game
          , maxUnlocked =
              if game.finished then
                Basics.max model.maxUnlocked <| model.level + 1
              else
                model.maxUnlocked
          }
        , Cmd.none
        )

    Restart ->
      newLevel model.level model

    NextLevel ->
      newLevel (model.level + 1) model

    PreviousLevel ->
      newLevel (model.level - 1) model


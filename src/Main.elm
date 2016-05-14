
module Main exposing (main)


{-|
# Game of logic

Copyright Théo Zimmermann 2016. License MPL 2.0

-}


import Time exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Game exposing (levelMax)


{- # MAIN -}


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }


{- # MODEL -}


type alias Model =
  { game : Game.Model
  , maxUnlocked : Int
  , level : Int
  }


init : (Model, Cmd Msg)
init =
  let
    (game, cmd) = Game.init 0
  in
  ( { game = game
    , maxUnlocked = 0
    , level = 0
    }
  , Cmd.map GameMsg cmd )


newLevel : Int -> Model -> (Model, Cmd Msg)
newLevel index model =
  let
    (game, cmd) = Game.init index
  in
  ( { model |
      game = game
    , level = index
    }
  , Cmd.map GameMsg cmd )


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
    , App.map GameMsg (Game.view model.game)
    ]


{- # ACTIONS AND UPDATE -}


type Msg
  = NoOp
  | GameMsg Game.Msg
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
        (game, cmd, finished) = Game.update action model.game
      in
        ( { model |
            game = game
          , maxUnlocked =
              if finished then
                Basics.max model.maxUnlocked <| model.level + 1
              else
                model.maxUnlocked
          }
        , Cmd.map GameMsg cmd
        )

    Restart ->
      newLevel model.level model

    NextLevel ->
      newLevel (model.level + 1) model

    PreviousLevel ->
      newLevel (model.level - 1) model


module Game.View (view) where


import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import DragDrop exposing (..)
import Style exposing (..)
import Game.Actions exposing (..)
import Game.Model exposing (Model)
import Game.Formula as Formula exposing (Formula)
import Game.Context as Context exposing (Context)


viewContext : Maybe Formula -> Signal.Address Action -> Context Formula -> Html
viewContext selected address context =
  div
    [ style
        [ greenBorder
        , smallPadding
        , bigBottomPadding
        ]
    ]
    [ div [] [ text <| "Assume: " ++ Formula.toString context.hypothesis ]
    , Context.view selected (Signal.forwardTo address <| always NoOp) context
    ]

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [("margin" , "10px")]
    ] <|
    [ Context.view
        model.selected
        (Signal.forwardTo address ContextAction)
        model.mainContext
    ] ++
    List.map (viewContext model.selected address) model.contexts ++
    [ div
        [ style  [ smallMargin , fixedHeight ] ]
        [ text <| "The goal is to get " ++ Formula.toString model.goal ++ "." ]
    , div
        [ style  [ fixedHeight , italic ] ]
        [ text <| model.message ]
    ]


module Game.View (view) where


import Maybe exposing (andThen)
import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import DragDrop exposing (..)
import Game.Actions exposing (..)
import Game.Model exposing (Model)
import Game.Formula as Formula exposing (Formula)
import Game.Context exposing (Context)


smallMargin = ("margin" , "4px 0")
smallPadding = ("padding" , "2px")
bigBottomPadding = ("padding-bottom" , "30px" )
grayBackground = ("background-color" , "gray")
redBorder = ("border" , "2px solid red")
greenBorder = ("border", "4px solid green")
fixedHeight = ("height" , "30px")
italic = ("font-style" , "italic")


viewFormula : Signal.Address Action -> Maybe Formula -> Int -> Formula -> Html
viewFormula address selected index formula =
  let
    combined = selected `andThen` Formula.combine formula

    dropOk = combined /= Nothing

    dropAction =
      case combined of
        Just f ->
          Drop (index , f)

        Nothing ->
          NoOp

  in

  div
    [ style
        [ smallMargin
        , smallPadding
        , fixedHeight
        ]
    ]
    [ span
      [ draggable "true"
      , onDragStart address <| DragStart (index , formula)
      , onDragOver dropOk address NoOp
      , onDrop address dropAction
      , style
          [ grayBackground
          , redBorder
          , smallPadding
          ]
      ]
      [ text <| Formula.toString formula ]
    ]


viewContext : Signal.Address Action -> Maybe Formula -> Context -> Html
viewContext address selected context =
  div
    [ style
        [ greenBorder
        , smallPadding
        , bigBottomPadding
        ]
    ]
    [ div [] [ text <| "Assume: " ++ Formula.toString context.hypothesis ]
    , div [] []
    ]

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [("margin" , "10px")]
    ] <|
    [ div []
        (Array.toList <| Array.indexedMap (viewFormula address model.selected) model.mainContext)
    ] ++
    List.map (viewContext address model.selected) model.contexts ++
    [ div
        [ style  [ smallMargin , fixedHeight ] ]
        [ text <| "The goal is to get " ++ Formula.toString model.goal ++ "." ]
    , div
        [ style  [ fixedHeight , italic ] ]
        [ text <| model.message ]
    ]


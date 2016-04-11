module Game.View (view) where


import Maybe exposing (andThen)
import Array
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode
import Native.DragDrop
import Game.Actions exposing (..)
import Game.Model exposing (Model)
import Game.Formula as Formula exposing (Formula)


{- ## Custom events -}


onDragOver : Bool -> Signal.Address a -> a -> Attribute
onDragOver dropOk addr msg =
  onWithOptions "dragover" {preventDefault = dropOk, stopPropagation = False} (Json.Decode.succeed ()) (\_ -> Signal.message addr msg)


onDrop : Signal.Address a -> a -> Attribute
onDrop addr msg =
  on "drop" (Json.Decode.succeed ()) (\_ -> Signal.message addr msg)


onDragStart : Signal.Address a -> a -> Attribute
onDragStart addr msg =
  Native.DragDrop.onDragStart {preventDefault = False, stopPropagation = False} (Json.Decode.succeed ()) (\_ -> Signal.message addr msg)


{- # VIEW -}


smallMargin = ("margin" , "4px 0")
smallPadding = ("padding" , "2px")
grayBackground = ("background-color" , "gray")
redBorder = ("border" , "2px solid red")
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


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [("margin" , "10px")]
    ]
    [ div []
        (Array.toList <| Array.indexedMap (viewFormula address model.selected) model.context)
    , div [ style  [ smallMargin , fixedHeight ] ] [ text "The goal is to get D." ]
    , div
        [ style  [ fixedHeight , italic ] ]
        [ text <| model.message ]
    ]


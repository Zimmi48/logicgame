module Game.View (view) where


import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import DragDrop exposing (..)
import Style exposing (..)
import Game.Actions exposing (..)
import Game.Model exposing (Model)
import Game.Formula as Formula exposing (Formula(..))
import Game.Context as Context exposing (Context)


viewContext : Maybe Formula -> Maybe Formula -> Signal.Address Action -> Int -> Context Formula -> Html
viewContext selectionContext selected address index context =
  let
    selected =
      case selectionContext of
        Nothing ->
          selected

        Just hypothesis ->
          if hypothesis == context.hypothesis then
            selected

          else
            Nothing

    dropAttributes =
      case (selectionContext , selected) of
        (Nothing , Just formula) ->
          let
            resultOfMove =
              case formula of
                Impl f1 f2 ->
                  if f1 == context.hypothesis then f2 else formula

                _ ->
                  formula

          in

          [ onDragOver Copy address NoOp
          , onDrop
              address
              (ContextAction index context.hypothesis <| Context.AddFormula resultOfMove)
          ]

        _ ->
          []
  in

  div
    ([ style
        [ greenBorder
        , smallMargin
        , smallPadding
        , bigBottomPadding
        ]
    ] ++ dropAttributes )
    [ div [] [ text <| "Assume: " ++ Formula.toString context.hypothesis ]
    , Context.view
        selected
        (Signal.forwardTo address <| ContextAction index context.hypothesis)
        context
    ]


viewMainContext : Maybe Formula -> Maybe Formula -> Signal.Address Action -> Context () -> Html
viewMainContext selectionContext selected address context =
  let
    dropAttributes =
      case (selectionContext , selected) of
        (Just f1 , Just f2) ->
          [ onDragOver Copy address NoOp
          , onDrop address (MainContextAction <| Context.AddFormula (Impl f1 f2))
          ]

        _ ->
          []

  in

  div
    dropAttributes
    [ Context.view
        (if selectionContext == Nothing then selected else Nothing)
        (Signal.forwardTo address MainContextAction)
        context
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [("margin" , "10px")]
    ] <|
    [ viewMainContext model.selectionContext model.selected address model.mainContext
    ] ++
    Array.toList
      (Array.indexedMap
        (viewContext model.selectionContext model.selected address)
        model.contexts
      ) ++
    [ div
        [ style  [ smallMargin , fixedHeight ] ]
        [ text <| "The goal is to get " ++ Formula.toString model.goal ++ "." ]
    , div
        [ style  [ fixedHeight , italic ] ]
        [ text <| model.message ]
    ]


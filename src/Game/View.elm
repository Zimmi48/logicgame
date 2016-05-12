module Game.View exposing (view)


import Array
import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import DragDrop exposing (..)
import Style exposing (..)
import Game.Actions exposing (..)
import Game.Model exposing (Model)
import Game.Formula as Formula exposing (Formula(..))
import Game.Context as Context exposing (Context)


viewContext : Maybe Formula -> Maybe Formula -> Int -> Context Formula -> Html Action
viewContext selectionContext selected index context =
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

          [ onDragOver Copy NoOp
          , onDrop (ContextAction index context.hypothesis <| Context.AddFormula resultOfMove)
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
        context
      |> Html.App.map (ContextAction index context.hypothesis)
    ]


viewMainContext : Maybe Formula -> Maybe Formula -> Context () -> Html Action
viewMainContext selectionContext selected context =
  let
    dropAttributes =
      case (selectionContext , selected) of
        (Just f1 , Just f2) ->
          [ onDragOver Copy NoOp
          , onDrop (MainContextAction <| Context.AddFormula (Impl f1 f2))
          ]

        _ ->
          []

  in

  div
    dropAttributes
    [ Context.view
        (if selectionContext == Nothing then selected else Nothing)
        context
      |> Html.App.map MainContextAction
    ]


view : Model -> Html Action
view model =
  div
    [ style [("margin" , "10px")]
    ] <|
    [ viewMainContext model.selectionContext model.selected model.mainContext
    ] ++
    Array.toList
      (Array.indexedMap
        (viewContext model.selectionContext model.selected)
        model.contexts
      ) ++
    [ div
        [ style  [ smallMargin , fixedHeight ] ]
        [ text <| "The goal is to get " ++ Formula.toString model.goal ++ "." ]
    , div
        [ style  [ fixedHeight , italic ] ]
        [ text <| model.message ]
    ]


module DragDrop (..) where


import Html exposing (Attribute)
import Html.Events exposing (onWithOptions)
import Json.Decode
import Native.DragDrop


{- ## Custom events -}


-- the boolean argument tells if it is possible to drop the currently dragged element on the element it is dragged over
onDragOver : Bool -> Signal.Address a -> a -> Attribute
onDragOver dropOk addr msg =
  onWithOptions
    "dragover"
    {preventDefault = dropOk, stopPropagation = True}
    (Json.Decode.succeed ())
    (\_ -> Signal.message addr msg)


onDrop : Signal.Address a -> a -> Attribute
onDrop addr msg =
  onWithOptions
    "drop"
    {preventDefault = False, stopPropagation = True}
    (Json.Decode.succeed ())
    (\_ -> Signal.message addr msg)


-- to emit the event, the element needs to have the attribute draggable
onDragStart : Signal.Address a -> a -> Attribute
onDragStart addr msg =
  Native.DragDrop.onDragStart
    {preventDefault = False, stopPropagation = False}
    (Json.Decode.succeed ())
    (\_ -> Signal.message addr msg)

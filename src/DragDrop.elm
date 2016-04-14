module DragDrop (..) where


import Html exposing (Attribute)
import Html.Events exposing (onWithOptions)
import Json.Decode
import Native.DragDrop


{- ## Custom events -}


type DropEffect
  = All
  | Copy
  | Link
  | Move
  | None


toString : DropEffect -> String
toString dropEffect =
  case dropEffect of
    All ->
      "all"

    Copy ->
      "copy"

    Link ->
      "link"

    Move ->
      "move"

    None ->
      "none"


onDragOver : DropEffect -> Signal.Address a -> a -> Attribute
onDragOver dropEffect addr msg =
  Native.DragDrop.onDragOver
    { preventDefault = True
    , stopPropagation = True
    , dropEffect = toString dropEffect
    }
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

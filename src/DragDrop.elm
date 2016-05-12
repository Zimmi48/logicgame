module DragDrop exposing (..)


import Html exposing (Attribute)
import Html.Events exposing (onWithOptions)
import Json.Decode
-- import Native.DragDrop


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


onDragOver : DropEffect -> a -> Attribute a
onDragOver dropEffect msg =
  onWithOptions
    "dragover"
     { preventDefault = True, stopPropagation = True }
     (Json.Decode.succeed msg)
{-
  Native.DragDrop.onDragOver
    { preventDefault = True
    , stopPropagation = True
    , dropEffect = toString dropEffect
    }
    (Json.Decode.succeed ())
    (\_ -> Signal.message addr msg)
-}

onDrop : a -> Attribute a
onDrop msg =
  onWithOptions
    "drop"
    {preventDefault = False, stopPropagation = True}
    (Json.Decode.succeed msg)


-- to emit the event, the element needs to have the attribute draggable
onDragStart : a -> Attribute a
onDragStart msg =
  onWithOptions
    "dragstart"
    { preventDefault = False, stopPropagation = False }
    (Json.Decode.succeed msg)
{-
  Native.DragDrop.onDragStart
    {preventDefault = False, stopPropagation = False}
    (Json.Decode.succeed ())
    (\_ -> Signal.message addr msg)
-}



module Game.Actions (..) where


import Time exposing (Time)
import Game.Formula exposing (Formula)
import Game.Context as Context


type Action
  = NoOp
  | MainContextAction Context.Action
  | ContextAction Int Formula Context.Action
  | Time Time

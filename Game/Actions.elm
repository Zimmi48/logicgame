module Game.Actions (..) where


import Game.Context as Context


type Action
  = NoOp
  | ContextAction Context.Action

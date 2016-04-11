module Game.Actions (..) where


import Game.Formula exposing (Formula)


type Action
  = NoOp
  | DragStart (Int, Formula)
  | Drop (Int, Formula)

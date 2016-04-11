module Game.Actions (..) where


import Game.Formula as Formula exposing (Formula)


type Action
  = NoOp
  | FormulaAction Int Formula Formula.Action

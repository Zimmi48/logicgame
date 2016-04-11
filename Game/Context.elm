module Game.Context (..) where


import Array exposing (Array)
import Game.Formula exposing (Formula)


type alias Context =
  { hypothesis : Formula
  , formulas : Array Formula
  }


module Game.Model (levelMax, Model, init) where


import Array exposing (Array)
import Game.Formula exposing (Formula(..))


levelMax = 1


levels : Int -> List Formula
levels index =
  let
    a = Var "A"
    b = Var "B"
    c = Var "C"
    d = Var "D"
  in
  if index == 0 then
    [ Impl a b
    , Impl a (Impl c d)
    , a
    , Impl b c
    ]

  else
    [ Impl a b
    , Impl c b
    , Impl c d
    , b
    , Impl (Impl a b) (Impl b c)
    ]


type alias Model =
  { context : Array Formula
  , goal : Formula
  , selected : Maybe Formula
  , message : String
  , finished : Bool
  }


init : Int -> Model
init level =
  { context =
      Array.fromList <| levels level
  , goal = Var "D"
  , selected = Nothing
  , message =
      if level == 0 then "Try moving A on A ⇒ B." else ""
  , finished = False
  }

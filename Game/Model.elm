module Game.Model (levelMax, Model, init) where


import Array exposing (Array)
import Game.Formula exposing (Formula(..))
import Game.Context exposing (Context)


levelMax = 2


type alias Model =
  { mainContext : Array Formula
  , contexts : List Context
  , goal : Formula
  , message : String
  , selected : Maybe Formula
  , finished : Bool
  }


init : Int -> Model
init level =
  let
    a = Var "A"
    b = Var "B"
    c = Var "C"
    d = Var "D"
  in

  if level == 0 then
    { mainContext =
        Array.fromList
          [ Impl a b
          , Impl a (Impl c d)
          , a
          , Impl b c
          ]
    , contexts = []
    , goal = Var "D"
    , message = "Try moving A on A ⇒ B."
    , selected = Nothing
    , finished = False
    }

  else if level == 1 then
    { mainContext =
        Array.fromList
          [ Impl a b
          , Impl c b
          , Impl c d
          , b
          , Impl (Impl a b) (Impl b c)
          ]
    , contexts = []
    , goal = Var "D"
    , message = ""
    , selected = Nothing
    , finished = False
    }

  else
    { mainContext =
        Array.fromList
          [ Impl a b
          , Impl (Impl a c) a
          , Impl b c
          , Impl a d
          ]
    , contexts = [ { hypothesis = a , formulas = Array.empty } ]
    , goal = Var "D"
    , message = "You can drag and drop any formula into the green context. Some will just be copied, others will be transformed."
    , selected = Nothing
    , finished = False
    }

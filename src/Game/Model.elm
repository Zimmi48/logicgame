module Game.Model (levelMax, Model, init) where


import Array exposing (Array)
import Game.Formula exposing (Formula(..))
import Game.Context as Context exposing (Context)


levelMax = 2


type alias Model =
  { mainContext : Context ()
  , contexts : Array (Context Formula)
  , goal : Formula
  , message : String
  , selected : Maybe Formula
  , selectionContext : Maybe Formula
  , finished : Bool
  }


getAModel { mainContext , contexts , message } =
  { mainContext = mainContext
  , contexts = Array.fromList contexts
  , goal = Var "D"
  , message = message
  , selected = Nothing
  , selectionContext = Nothing
  , finished = False
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
        Context.fromList
          [ Impl a b
          , Impl a (Impl c d)
          , a
          , Impl b c
          ]
    , contexts = []
    , message = "Try moving A on A ⇒ B."
    } |> getAModel

  else if level == 1 then
    { mainContext =
        Context.fromList
          [ Impl a b
          , Impl c b
          , Impl c d
          , b
          , Impl (Impl a b) (Impl b c)
          ]
    , contexts = []
    , message = ""
    } |> getAModel

  else
    { mainContext =
        Context.fromList
          [ Impl a b
          , Impl (Impl a c) a
          , Impl b c
          , Impl a d
          ]
    , contexts = [ Context.empty a ]
    , message = "You can drag and drop any formula into the green context. Some will just be copied, others will be transformed."
    } |> getAModel

module Game.Model (levelMax, Model, init, hints) where


import Array exposing (Array)
import Time exposing (Time)
import Game.Formula exposing (Formula(..))
import Game.Context as Context exposing (Context)


levelMax = 3


type alias Model =
  { mainContext : Context ()
  , contexts : Array (Context Formula)
  , goal : Formula
  , hints : Hints
  , message : String
  , selected : Maybe Formula
  , selectionContext : Maybe Formula
  , finished : Bool
  }


type Hints = Hints (Time -> Model -> String)


hintsOfHints : Hints -> Time -> Model -> String
hintsOfHints (Hints hints) = hints


hints : Time -> Model -> String
hints time model = (hintsOfHints model.hints) time model


a = Var "A"
b = Var "B"
c = Var "C"
d = Var "D"


defaultModel : Model
defaultModel =
  { mainContext = Context.fromList []
  , contexts = Array.empty
  , goal = d
  , hints = Hints (\_ _ -> "")
  , message = ""
  , selected = Nothing
  , selectionContext = Nothing
  , finished = False
  }


init : Int -> Model
init level =
  if level == 0 then
    { defaultModel |
      mainContext =
        Context.fromList
          [ Impl a b
          , Impl a (Impl c d)
          , a
          , Impl b c
          ]
    , hints = Hints (\_ _ -> "Try moving A on A ⇒ B.")
    }

  else if level == 1 then
    { defaultModel |
      mainContext =
        Context.fromList
          [ Impl a b
          , Impl c b
          , Impl c d
          , b
          , Impl (Impl a b) (Impl b c)
          ]
    }

  else if level == 2 then
    { defaultModel |
      mainContext =
        Context.fromList
          [ Impl a b
          , Impl (Impl a c) a
          , Impl b c
          , Impl a d
          ]
    , contexts = Array.fromList [ Context.empty a ]
    , hints =
        Hints
          (\_ _ ->
              "You can drag and drop any formula into the green context. Some will just be copied, others will be transformed."
          )
    }

  else
    { defaultModel |
      mainContext =
        Context.fromList
          [ Impl c d
          , Impl d c
          , Impl (Impl a c) (Impl (Impl b c) c)
          , Impl a d
          , Impl b a
          ]
    , contexts = Array.fromList [ Context.empty a , Context.empty b ]
    }

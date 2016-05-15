module Game.Model exposing
  ( levelMax
  , Model
  , init
  , Action(..)
  , HintStatus(..)
  , wait
  )
-- TODO : wait tasks must have a way to check that the level has not changed


import Array exposing (Array)
import Task
import Process
import Time exposing (Time, second)
import Game.Formula exposing (Formula(..))
import Game.Context as Context exposing (Context)


-- MODEL


levelMax = 3


type alias Model =
  { mainContext : Context ()
  , contexts : Array (Context Formula)
  , goal : Formula
  -- Hints: the game must have been in some condition for at least 3 seconds
  , hints :
      Array
        ( { mainContext : Context ()
          , contexts : Array (Context Formula)
          } -> Bool
        , String
        )
  , hintStatus : HintStatus
  , currentHint : String
  , selected : Maybe Formula
  , selectionContext : Maybe Formula
  , finished : Bool
  }


type HintStatus = NoHint | Waiting Int | Active Int


a = Var "A"
b = Var "B"
c = Var "C"
d = Var "D"


defaultModel : Model
defaultModel =
  { mainContext = Context.fromList []
  , contexts = Array.empty
  , goal = d
  , hints = Array.empty
  , hintStatus = NoHint
  , currentHint = ""
  , selected = Nothing
  , selectionContext = Nothing
  , finished = False
  }


wait : Int -> Cmd Action
wait index =
  Process.sleep (3 * second)
  |> Task.perform (always NoOp) (always <| ChangeHint index)

   
init : Int -> (Model, Cmd Action)
init level =
  let game = init1 level in
  ( game
  , case game.hintStatus of
      Waiting i ->
        wait i

      _ ->
        Cmd.none
  )


init1 level =
  if level == 0 then
    let
      condition1 { mainContext } =
        Array.get 0 mainContext.formulas == Just (Impl a b)
        && Array.get 2 mainContext.formulas == Just a

      condition2 { mainContext } =
        ( Array.get 0 mainContext.formulas == Just (Impl a b)
            && Array.get 2 mainContext.formulas == Just (Impl c d)
        )
        || ( Array.get 1 mainContext.formulas /= Just (Impl c d)
               && Array.get 2 mainContext.formulas == Just b
           )
    in
    { defaultModel |
      mainContext =
        Context.fromList
          [ Impl a b
          , Impl a (Impl c d)
          , a
          , Impl b c
          ]
    , hints =
        Array.fromList
          [ ( condition1 , "Try moving A on A ⇒ B." )
          , ( condition2
            , "I think you ran into troubles: you might want to restart."
            )
          ]
    , hintStatus = Waiting 0
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
    let
      condition1 { contexts } =
        Maybe.map (.formulas >> Array.isEmpty) (Array.get 0 contexts)
        == Just True

      condition2 { mainContext , contexts } =
        Array.length mainContext.formulas == 4
    in
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
        Array.fromList
          [ ( condition1
            , "You can drag and drop any formula into the green context. Some will just be copied, others will be transformed."
            )
          , ( condition2
            , "You can also drag any formula out of the green context. "
            )
          ]
    , hintStatus = Waiting 0
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


-- ACTIONS


type Action
  = NoOp
  | MainContextAction Context.Action
  | ContextAction Int Formula Context.Action
  | ChangeHint Int

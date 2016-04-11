module Game.Formula (Formula(..), combine, toString) where


-- Propositional calculus to start with


type Formula
  = Var String
  | Impl Formula Formula


-- Takes two formulas and return a resulting formula if they can be combined by modus ponens
combine : Formula -> Formula -> Maybe Formula
combine f f' =
  case (f , f') of
    (Impl f1 f2 , Impl f1' f2') ->
      if f1 == f' then
        Just f2
      else if f1' == f then
        Just f2'
      else
        Nothing

    (Impl f1 f2 , _) ->
      if f1 == f' then
        Just f2
      else
        Nothing

    (_ , Impl f1' f2') ->
      if f1' == f then
        Just f2'
      else
        Nothing

    _ -> Nothing


addPar : Formula -> String
addPar f =
  case f of
    Var s ->
      s

    _ ->
      "(" ++ toString f ++ ")"


toString : Formula -> String
toString f =
  case f of
    Var s ->
      s

    Impl f1 f2 ->
       addPar f1 ++ " ⇒ " ++ addPar f2

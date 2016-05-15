module Game.Update exposing (update)


import Array exposing (Array)
import Time
import Game.Formula as Formula exposing (Formula, Action(..))
import Game.Context as Context exposing (Context, Action(..))
import Game.Model exposing (Model, Action(..), HintStatus(..), wait)


addFormulaToMainContext : Formula -> Context.Action -> Model -> Model
addFormulaToMainContext formula action model =
  { model |
    mainContext = Context.update action model.mainContext
  , selected = Nothing
  , selectionContext = Nothing
  , finished = model.finished || formula == model.goal
  }


update : Game.Model.Action -> Model -> (Model, Cmd Game.Model.Action, Bool)
update action model =
  let (model, cmd) = update1 action model |> update2 in
  (model, cmd, model.finished)


update1 action model =
  case action of
    MainContextAction (FormulaAction _ formula Selected) ->
      { model |
        selected = Just formula
      , selectionContext = Nothing
      }
      
    MainContextAction (FormulaAction _ _ (Result formula) as action) ->
      addFormulaToMainContext formula action model

    MainContextAction (AddFormula formula as action) ->
      addFormulaToMainContext formula action model

    ContextAction _ hypothesis (FormulaAction _ formula Selected) ->
      { model |
        selected = Just formula
      , selectionContext = Just hypothesis
      }
      
    ContextAction index _ action ->
      { model |
        contexts =
          Array.indexedMap
            (\i c -> if i == index then Context.update action c else c)
            model.contexts
      }
      
    ChangeHint index ->
      if model.hintStatus == Waiting index then
        { model |
          hintStatus = Active index
        , currentHint =
            Maybe.withDefault ""
            <| Maybe.map snd
            <| Array.get index model.hints
        }
      else
        model
        
    _ ->
      model


update2 model =
  let
    reducedModel =
      { mainContext = model.mainContext
      , contexts = model.contexts
      }
      
    condition index =
      (Maybe.withDefault (always False)
         <| Maybe.map fst
         <| Array.get index model.hints)
        reducedModel
  in
  case model.hintStatus of
    Waiting index ->
      if condition index then
        ( model , Cmd.none )

      else
        update2
          { model |
            hintStatus = NoHint
          }

    Active index ->
      if condition index then
        ( model , Cmd.none )

      else
        update2
          { model |
            hintStatus = NoHint
          , currentHint = ""
          }

    NoHint ->
      let
        checkCond i (cond, _) =
          if cond reducedModel then Just i else Nothing
             
        verified =
          Maybe.oneOf
          <| Array.toList
          <| Array.indexedMap checkCond model.hints

      in
        case verified of
          Just i ->
            ( { model | hintStatus = Waiting i } , wait i )

          Nothing ->
            ( model , Cmd.none )

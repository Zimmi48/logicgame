module Game exposing (..)

import Game.Model
import Game.Actions
import Game.Update
import Game.View

type alias Model = Game.Model.Model
init = Game.Model.init

type alias Msg = Game.Actions.Action
time = Game.Actions.Time

update = Game.Update.update

view = Game.View.view

levelMax = levelMax

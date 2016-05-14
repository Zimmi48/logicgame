module Game exposing (..)

import Game.Model
import Game.Update
import Game.View

type alias Model = Game.Model.Model
init = Game.Model.init

type alias Msg = Game.Model.Action

update = Game.Update.update

view = Game.View.view

levelMax = levelMax

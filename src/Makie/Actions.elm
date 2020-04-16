module Makie.Actions exposing (Action, none)

import Makie.Internal.NewMakie as M exposing (Action)


type alias Action =
    Action


none : Action
none =
    M.NoAction

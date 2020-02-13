module Makie.Options exposing
    ( Options
    , default
    )

{-| This module provides API related to Options


# Types

@docs Options


# Constructor

@docs default

-}

import Makie.Internal.Makie as M


{-| Options type.
-}
type alias Options =
    M.Options


{-| Create default options
-}
default : Options
default =
    let
        renderOptions =
            { pointRadius = 5
            , selectedPointRadius = 10
            , borderWidth = 2
            , touchTolerance = 10
            }
    in
    { render = renderOptions }
        |> M.Options

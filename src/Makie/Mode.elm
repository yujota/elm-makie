module Makie.Mode exposing
    ( Mode
    , fromString, point, browse
    )

{-| This module provides API related to Mode


# Types

@docs Mode


# Constructor

@docs fromString, point, browse

-}

import Makie.Types.State as S


{-| Mode type.
-}
type alias Mode =
    S.Mode


{-| from string
-}
fromString : String -> Maybe Mode
fromString txt =
    if txt == "browse" then
        Just browse

    else if txt == "point" then
        Just point

    else
        Nothing


{-| browse mode
-}
browse : Mode
browse =
    S.BrowseMode


{-| point mode
-}
point : Mode
point =
    S.AnnotationMode S.PointAnnotationMode

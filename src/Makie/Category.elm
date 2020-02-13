module Makie.Category exposing
    ( Category, Id
    , id, category, categoryWith
    )

{-| This module provides API related to Category


# Types

@docs Category, Id


# Constructor

@docs id, category, categoryWith

-}

import Color exposing (Color)
import Makie.Internal.Color as Color
import Makie.Internal.Makie as M


{-| Category Id type.
-}
type alias Id =
    M.CategoryId


{-| Category type.
-}
type alias Category =
    M.Category


{-| Construct Id
-}
id : String -> Id
id =
    M.CategoryId


{-| Create category
-}
category : String -> Color -> Category
category name inputColor =
    let
        selectedColor =
            inputColor

        color =
            selectedColor |> Color.mapSaturation (\s -> max 0 (s - 0.2))

        fillColor =
            Color.mapAlpha (\a -> max 0.3 (a - 0.3))
    in
    { name = name
    , color = color
    , selectedColor = selectedColor
    , fillColor = fillColor color
    , selectedFillColor = fillColor selectedColor
    , borderColor = color
    , selectedBorderColor = selectedColor
    }
        |> M.Category


{-| Create category with options
-}
categoryWith :
    { name : String
    , color : Color
    , selectedColor : Color
    , fillColor : Color
    , selectedFillColor : Color
    , borderColor : Color
    , selectedBorderColor : Color
    }
    -> Category
categoryWith =
    M.Category

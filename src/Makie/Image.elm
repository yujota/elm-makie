module Makie.Image exposing
    ( Image, Id
    , id, image
    )

{-| This module provides API related to Image


# Types

@docs Image, Id


# Constructor

@docs id, image

-}

import Makie.Types.Data as D


{-| Annotation type.
-}
type alias Image =
    D.Image


{-| Image's ID type.
-}
type alias Id =
    D.ImageIdentifier


{-| Construct Id
-}
id : String -> Id
id =
    D.UserSpecifiedId


{-| Create image
-}
image : Id -> { name : String, url : String, width : Int, height : Int } -> Image
image i { name, url, width, height } =
    D.NormalImage { identifier = i, name = name, url = url, width = width, height = height }

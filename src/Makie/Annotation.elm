module Makie.Annotation exposing
    ( Annotation, Id
    , id, point
    )

{-| This module provides API related to Annotation


# Types

@docs Annotation, Id


# Constructor

@docs id, point

-}

import Makie.Category as Category
import Makie.Internal.Annotation as Annotation
import Makie.Internal.Makie as M
import Makie.Types.Geometry as G
import Time exposing (Posix)


{-| Annotation type.
-}
type alias Annotation =
    M.Annotation


{-| Annotation's ID type.
-}
type alias Id =
    M.AnnotationId


{-| Construct Id
-}
id : String -> Id
id =
    M.AnnotationId


{-| Create point annotation
-}
point : Posix -> Category.Id -> { x : Float, y : Float } -> Annotation
point createdTime (M.CategoryId categoryId) { x, y } =
    Annotation.point createdTime categoryId (G.pPoint x y)
        |> M.Annotation

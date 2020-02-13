module Makie.Internal.Makie exposing (..)

import Makie.Types.Annotation
import Makie.Types.Data exposing (Image(..))
import Makie.Types.State as S
import Random


type Makie msg
    = Makie (S.State msg)


type Options
    = Options S.Options


type Data
    = FancyData { data : Makie.Types.Data.Data, seed : Random.Seed }
    | CustomData Makie.Types.Data.Data


getData : Data -> Makie.Types.Data.Data
getData d =
    case d of
        FancyData { data } ->
            data

        CustomData data ->
            data


type Query
    = Add Annotation
    | Insert AnnotationId Annotation
    | Remove AnnotationId


type Status
    = Status String


type Annotation
    = Annotation Makie.Types.Annotation.Annotation


type AnnotationId
    = AnnotationId String


type CategoryId
    = CategoryId String


type Category
    = Category Makie.Types.Annotation.Category


type Project
    = Project Makie.Types.Annotation.Project

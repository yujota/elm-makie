module Makie.Data exposing
    ( Data, Status, Query
    , data, update
    )

{-| This module provides API related to Data


# Types

@docs Data, Status, Query


# Constructor

@docs data, update

-}

import Dict exposing (Dict)
import Makie.Annotation exposing (Annotation)
import Makie.Image exposing (Image)
import Makie.Internal.Makie as M
import Makie.Types.Data
import Random
import Uuid


{-| Data type.
-}
type alias Data =
    M.Data


{-| Status type.
-}
type alias Status =
    M.Status


{-| Query type.
-}
type alias Query =
    M.Query


{-| Constructor
-}
data : Int -> Image -> List Annotation -> Data
data seedValue img ants =
    let
        initSeed =
            Random.initialSeed seedValue

        f ant { seed, annotations } =
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.uuidGenerator seed

                ext (M.Annotation a) =
                    a
            in
            { seed = newSeed, annotations = ( Uuid.toString newUuid, ext ant ) :: annotations }

        ( s, antDict ) =
            List.foldr f { seed = initSeed, annotations = [] } ants
                |> (\r -> ( r.seed, Dict.fromList r.annotations ))

        ( currentStatus, currentSeed ) =
            Random.step Uuid.uuidGenerator s
                |> Tuple.mapFirst Uuid.toString

        history =
            {}
    in
    { status = currentStatus
    , image = img
    , annotations = antDict
    , history = history
    }
        |> (\d -> M.FancyData { data = d, seed = currentSeed })


{-| Update data by queries
-}
update : List Query -> Data -> Data
update queries data_ =
    case data_ of
        M.FancyData r ->
            update_ queries { seed = r.seed, data = r.data } |> M.FancyData

        M.CustomData _ ->
            data_



-- helper functions


update_ :
    List Query
    -> { seed : Random.Seed, data : Makie.Types.Data.Data }
    -> { seed : Random.Seed, data : Makie.Types.Data.Data }
update_ queries r =
    case List.isEmpty queries of
        True ->
            r

        False ->
            let
                f q { seed, annotations } =
                    case q of
                        M.Add (M.Annotation ant) ->
                            let
                                ( newId, newSeed ) =
                                    Random.step Uuid.uuidGenerator seed
                                        |> Tuple.mapFirst Uuid.toString
                            in
                            { seed = newSeed, annotations = Dict.insert newId ant r.data.annotations }

                        M.Insert (M.AnnotationId antId) (M.Annotation ant) ->
                            { seed = seed, annotations = Dict.insert antId ant r.data.annotations }

                        M.Remove (M.AnnotationId antId) ->
                            { seed = seed, annotations = Dict.remove antId annotations }

                ( s, updated ) =
                    List.foldr f { seed = r.seed, annotations = r.data.annotations } queries
                        |> (\x -> ( x.seed, x.annotations ))

                ( currentStatus, currentSeed ) =
                    Random.step Uuid.uuidGenerator s
                        |> Tuple.mapFirst Uuid.toString
            in
            { seed = currentSeed, data = r.data |> (\d -> { d | annotations = updated, status = currentStatus }) }

module Makie.Data.Internal.Data exposing (..)

import Dict exposing (Dict)


type Data
    = Data { project : Project, image : Image, annotations : Dict String Annotation }


type Project
    = Project


type Image
    = Image


type Annotation
    = Annotation


type Id
    = Id

module Makie.Data exposing (..)

import Makie.Data.Internal.Data as D exposing (Annotation, Data, Id, Image, Project)


type alias Data =
    Data


type alias Project =
    Project


type alias Image =
    Image


type alias Annotation =
    Annotation


type alias Id =
    Id


data :
    { project : Project
    , image : Image
    , annotations : List { id : Id, category : Id, annotation : Annotation }
    }
    -> Data
data =
    Debug.todo "hoge"


getProject : Data -> Project
getProject (D.Data r) =
    r.project


{-| Failed when given project has no category for existing annotation
-}
setProject : Project -> Data -> Result String Data
setProject _ (D.Data r) =
    Err "Failed"


getImage : Data -> Image
getImage (D.Data r) =
    r.image


setImage : Data -> Image
setImage (D.Data r) =
    r.image


getAnnotations : Data -> List { id : Id, category : Id, annotation : Annotation }
getAnnotations (D.Data r) =
    Debug.todo "hoge"


{-| Erase existing data
-}
setAnnotations : List { id : Id, category : Id, annotation : Annotation } -> Data -> Data
setAnnotations l (D.Data r) =
    Debug.todo "hoge"


get : Id -> Data -> Maybe { category : Id, annotation : Annotation }
get i (D.Data r) =
    Debug.todo "Not implemented"


member : Id -> Data -> Bool
member i (D.Data r) =
    Debug.todo "Not implemented"


insert : Id -> { category : Id, annotation : Annotation } -> Data -> Data
insert =
    Debug.todo "Not implemented"


remove : Id -> Data -> Data
remove =
    Debug.todo "Not implemented"

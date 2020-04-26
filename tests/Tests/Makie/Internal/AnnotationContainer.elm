module Tests.Makie.Internal.AnnotationContainer exposing (..)

import Binary
import BoundingBox2d
import Expect exposing (equal, equalLists)
import Makie.Internal.Makie as M
import Makie.Internal.ObjectContainer as AC
import Test exposing (Test, describe, test)


testGetQuadKey : Test
testGetQuadKey =
    let
        testExampleOne () =
            let
                actual =
                    AC.getQuadKey 13 (M.imagePoint { x = 48, y = 80 })
                        |> Binary.toDecimal

                desired =
                    45
            in
            equal actual desired
    in
    describe "test Makie.Internal.ObjectContainer.getQuadKey" [ test "test Example 1" testExampleOne ]


testGetLinerQuaternaryTreeIndex : Test
testGetLinerQuaternaryTreeIndex =
    let
        testExampleOne () =
            let
                boundingBox =
                    BoundingBox2d.from (M.imagePoint { x = 10, y = 20 }) (M.imagePoint { x = 70, y = 80 })

                actual =
                    AC.getLinerQuaternaryTreeIndex { depth = 2, unitSize = 100 } boundingBox

                desired =
                    1
            in
            equal actual desired

        testExampleTwo () =
            let
                boundingBox =
                    BoundingBox2d.from (M.imagePoint { x = 10, y = 20 }) (M.imagePoint { x = 70, y = 80 })

                actual =
                    AC.getLinerQuaternaryTreeIndex { depth = 2, unitSize = 50 } boundingBox

                desired =
                    0
            in
            equal actual desired

        testExampleThree () =
            let
                boundingBox =
                    BoundingBox2d.from (M.imagePoint { x = 55, y = 15 }) (M.imagePoint { x = 75, y = 35 })

                actual =
                    AC.getLinerQuaternaryTreeIndex { depth = 4, unitSize = 10 } boundingBox

                desired =
                    2
            in
            equal actual desired

        testExampleFour () =
            let
                boundingBox =
                    BoundingBox2d.from (M.imagePoint { x = 65, y = 25 }) (M.imagePoint { x = 75, y = 35 })

                actual =
                    AC.getLinerQuaternaryTreeIndex { depth = 4, unitSize = 10 } boundingBox

                desired =
                    12
            in
            equal actual desired

        testExampleFive () =
            let
                boundingBox =
                    BoundingBox2d.from (M.imagePoint { x = 32, y = 12 }) (M.imagePoint { x = 37, y = 18 })

                actual =
                    AC.getLinerQuaternaryTreeIndex { depth = 4, unitSize = 10 } boundingBox

                desired =
                    28
            in
            equal actual desired

        testExampleSix () =
            let
                boundingBox =
                    BoundingBox2d.from (M.imagePoint { x = 35, y = 35 }) (M.imagePoint { x = 45, y = 45 })

                actual =
                    AC.getLinerQuaternaryTreeIndex { depth = 4, unitSize = 10 } boundingBox

                desired =
                    0
            in
            equal actual desired

        testExampleSeven () =
            let
                boundingBox =
                    BoundingBox2d.from (M.imagePoint { x = 15, y = 65 }) (M.imagePoint { x = 25, y = 65 })

                actual =
                    AC.getLinerQuaternaryTreeIndex { depth = 4, unitSize = 10 } boundingBox

                desired =
                    3
            in
            equal actual desired
    in
    describe "test Makie.Internal.ObjectContainer.getLinerQuaternaryTreeIndex"
        [ test "test Example 1" testExampleOne
        , test "test Example 2" testExampleTwo
        , test "test Example 3" testExampleThree
        , test "test Example 4" testExampleFour
        , test "test Example 5" testExampleFive
        , test "test Example 6" testExampleSix
        , test "test Example 7" testExampleSeven
        ]


testGetIndicesTouched : Test
testGetIndicesTouched =
    let
        testExampleOne () =
            let
                container =
                    AC.objectContainer { depth = 2, unitSize = 100 }

                boundingBox =
                    BoundingBox2d.from (M.imagePoint { x = 10, y = 20 }) (M.imagePoint { x = 70, y = 80 })

                actual =
                    AC.getIndicesTouched boundingBox container

                desired =
                    [ 0, 1 ]
            in
            equalLists actual desired

        testExampleTwo () =
            let
                container =
                    AC.objectContainer { depth = 3, unitSize = 100 }

                boundingBox =
                    BoundingBox2d.from (M.imagePoint { x = 10, y = 20 }) (M.imagePoint { x = 70, y = 80 })

                actual =
                    AC.getIndicesTouched boundingBox container

                desired =
                    [ 0, 1, 5 ]
            in
            equalLists actual desired

        testExampleThree () =
            let
                container =
                    AC.objectContainer { depth = 3, unitSize = 10 }

                boundingBox =
                    BoundingBox2d.from (M.imagePoint { x = 15, y = 15 }) (M.imagePoint { x = 25, y = 15 })

                actual =
                    AC.getIndicesTouched boundingBox container

                desired =
                    [ 0, 1, 2, 8, 11 ]
            in
            equalLists actual desired
    in
    describe "test Makie.Internal.ObjectContainer.getIndicesTouched"
        [ test "test Example 1" testExampleOne
        , test "test Example 2" testExampleTwo
        , test "test Example 3" testExampleThree
        ]

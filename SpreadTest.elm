module SpreadTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import List.Extra
import Spread
import Test exposing (Test, describe)


-- generic helpers


{-| [a, b, c] -> [( a, b ), ( a, c ), ( b, c )]
-}
getAllPairCombinations : List a -> List ( a, a )
getAllPairCombinations list =
    case list of
        [] ->
            []

        x :: xs ->
            List.map ((,) x) xs ++ getAllPairCombinations xs



-- Boxes


type alias Box =
    { id : Int
    , y : Float
    , size : Float
    }


spread =
    Spread.spread
        { getPosition = .y
        , setPosition = \y box -> { box | y = y }
        , getSize = .size
        }



-- Fuzzers


yFuzzer =
    Fuzz.intRange -100000 100000
        |> Fuzz.map (\n -> toFloat n / 100)


sizeFuzzer =
    Fuzz.intRange 1 20000
        |> Fuzz.map (\n -> toFloat n / 100)


boxFuzzer : Fuzzer Box
boxFuzzer =
    Fuzz.map3 Box
        (Fuzz.intRange 0 <| round 1.0e9)
        yFuzzer
        sizeFuzzer


makeParams : Float -> Float -> List Box -> ( Float, Float, List Box )
makeParams y size boxes =
    let
        min =
            y

        -- max > min, and there is enough space to fit all boxes
        max =
            boxes
                |> List.map .size
                |> List.sum
                |> (+) min
                |> (+) size
    in
        ( min, max, boxes )


paramsFuzzer : Fuzzer ( Float, Float, List Box )
paramsFuzzer =
    Fuzz.map3 makeParams
        yFuzzer
        sizeFuzzer
        (Fuzz.list boxFuzzer)


makeNonCollidingParams : Float -> Float -> List ( Float, Float ) -> ( Float, Float, List Box )
makeNonCollidingParams min maxOffset pairs =
    let
        foldPair ( distanceFromTop, size ) ( boxes, top ) =
            let
                box =
                    { id = 0
                    , y = top + distanceFromTop + size / 2
                    , size = size
                    }
            in
                ( box :: boxes, box.y + size / 2 )

        ( boxes, top ) =
            List.foldr foldPair ( [], min ) pairs
    in
        ( min, top + maxOffset, boxes )


nonCollidingParamsFuzzer : Fuzzer ( Float, Float, List Box )
nonCollidingParamsFuzzer =
    Fuzz.map3 makeNonCollidingParams
        yFuzzer
        sizeFuzzer
        (Fuzz.list (Fuzz.map2 (,) sizeFuzzer sizeFuzzer))



-- tests


maintainsOriginalYOrder =
    Test.fuzz paramsFuzzer "Keeps boxes in the same order as the original positions" <|
        \( min, max, originalBoxes ) ->
            let
                uniques =
                    originalBoxes
                        |> List.Extra.uniqueBy .y

                originalYOrder =
                    uniques
                        |> List.sortBy .y
                        |> List.map .id

                resultYOrder =
                    uniques
                        |> spread min max
                        |> List.sortBy .y
                        |> List.map .id
            in
                Expect.equalLists originalYOrder resultYOrder


resolvesBoxCollisions =
    Test.fuzz paramsFuzzer "Resolves boxes collisions" <|
        \( min, max, boxes ) ->
            let
                spreadBoxes =
                    spread min max boxes

                -- allow for non-significant floating point errors
                boxesCollide ( a, b ) =
                    abs (a.y - b.y) - (a.size + b.size) / 2 >= -1.0e-8
            in
                spreadBoxes
                    |> getAllPairCombinations
                    |> List.all boxesCollide
                    |> Expect.true "boxes should not collide"


keepsBoxesWithoutCollisionsWhereTheyAre =
    Test.fuzz nonCollidingParamsFuzzer "Keeps boxes without collisions where they are" <|
        \( min, max, originalBoxes ) ->
            let
                resultBoxes =
                    spread min max originalBoxes

                haveSameY ( originalBox, resultBox ) =
                    abs (resultBox.y - originalBox.y) <= 1.0e-8
            in
                List.map2 (,)
                    (List.sortBy .y originalBoxes)
                    (List.sortBy .y resultBoxes)
                    |> List.all haveSameY
                    |> Expect.true ""


movesBoxesWithinBorders =
    Test.fuzz paramsFuzzer "Moves all boxes inside min and max" <|
        \( min, max, boxes ) ->
            let
                spreadBoxes =
                    spread min max boxes

                -- allow for non-significant floating point errors
                boxIsWithin box =
                    box.y + box.size / 2 - max <= 1.0e-8 && box.y - box.size / 2 - min >= -1.0e-8
            in
                spreadBoxes
                    |> List.all boxIsWithin
                    |> Expect.true "all boxes should be inside the borders"


all : Test
all =
    describe "Spread.spread"
        [ maintainsOriginalYOrder
        , resolvesBoxCollisions
        , keepsBoxesWithoutCollisionsWhereTheyAre
        , movesBoxesWithinBorders
        ]

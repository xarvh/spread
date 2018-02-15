module Spread exposing (Args, spread)

import List.Extra


-- API


spread : Args box -> Float -> Float -> List box -> List box
spread args minY maxY points =
    points
        |> List.map (pointToStack args)
        |> spreadStacks minY maxY
        |> List.map (breakStack args)
        |> List.concat


{-| getPosition and setPosition refer to the *middle* of the box
-}
type alias Args box =
    { getPosition : box -> Float
    , setPosition : Float -> box -> box
    , getSize : box -> Float
    }



-- Stacks
--
-- A Stack is just a pile of non-overlapping boxes


type alias Stack reference =
    { topY : Float
    , bottomY : Float
    , references : List reference
    }


stackHeight : Stack a -> Float
stackHeight stack =
    stack.topY - stack.bottomY


stackMiddleY : Stack a -> Float
stackMiddleY stack =
    (stack.topY + stack.bottomY) / 2


pointToStack : Args box -> box -> Stack box
pointToStack args point =
    let
        y =
            args.getPosition point

        size =
            args.getSize point
    in
        { topY = y + size / 2
        , bottomY = y - size / 2
        , references = [ point ]
        }


breakStack : Args a -> Stack a -> List a
breakStack args stack =
    let
        firstAvailableY =
            stack.topY

        foldReference reference ( availableY, references ) =
            let
                size =
                    args.getSize reference

                referenceMiddleY =
                    availableY - size / 2

                nextAvailableY =
                    availableY - size

                newReference =
                    args.setPosition referenceMiddleY reference
            in
                ( nextAvailableY, newReference :: references )

        ( availableY, newReferences ) =
            stack.references
                |> List.foldr foldReference ( firstAvailableY, [] )
    in
        newReferences



-- Main algorithm


spreadStacks : Float -> Float -> List (Stack a) -> List (Stack a)
spreadStacks minY maxY unsortedStacks =
    let
        sortedStacks =
            unsortedStacks
                |> List.sortBy stackMiddleY

        ( bottomStacks, newMin, nonBottomStacks ) =
            clampMin [] minY sortedStacks

        ( topStacks, newMax, middleStacks ) =
            clampMax [] maxY (List.reverse nonBottomStacks)

        ( resolvedMiddleStacks, middleStacksHaveExpanded ) =
            resolveOverlaps middleStacks

        spreadMiddleStacks =
            if newMax == maxY && newMin == minY && not middleStacksHaveExpanded then
                resolvedMiddleStacks
            else
                spreadStacks newMin newMax resolvedMiddleStacks
    in
        List.concat
            [ topStacks
            , bottomStacks
            , spreadMiddleStacks
            ]



-- Clamping


clampMin : List (Stack a) -> Float -> List (Stack a) -> ( List (Stack a), Float, List (Stack a) )
clampMin previouslyClampedStacks minY sortedStacks =
    case sortedStacks of
        [] ->
            ( previouslyClampedStacks, minY, sortedStacks )

        stack :: ss ->
            if stack.bottomY >= minY then
                ( previouslyClampedStacks, minY, sortedStacks )
            else
                let
                    clampedStack =
                        { stack | bottomY = minY, topY = minY + stackHeight stack }
                in
                    clampMin (clampedStack :: previouslyClampedStacks) clampedStack.topY ss


clampMax : List (Stack a) -> Float -> List (Stack a) -> ( List (Stack a), Float, List (Stack a) )
clampMax previouslyClampedStacks maxY sortedStacks =
    case sortedStacks of
        [] ->
            ( previouslyClampedStacks, maxY, sortedStacks )

        stack :: ss ->
            if stack.topY <= maxY then
                ( previouslyClampedStacks, maxY, sortedStacks )
            else
                let
                    clampedStack =
                        { stack | bottomY = maxY - stackHeight stack, topY = maxY }
                in
                    clampMax (clampedStack :: previouslyClampedStacks) clampedStack.bottomY ss



-- Overlaps


resolveOverlaps : List (Stack a) -> ( List (Stack a), Bool )
resolveOverlaps oldStacks =
    let
        stacksAreOverlapping a b =
            a.topY > b.bottomY

        newStacks =
            oldStacks
                |> List.sortBy stackMiddleY
                |> List.Extra.groupWhileTransitively stacksAreOverlapping
                |> List.map mergeSortedStacks
    in
        ( newStacks, List.length newStacks /= List.length oldStacks )


mergeSortedStacks : List (Stack a) -> Stack a
mergeSortedStacks sortedStacks =
    let
        -- weighted average of stack middle y
        sumOfYsWeightedByHeight =
            sortedStacks
                |> List.map (\stack -> stackMiddleY stack * stackHeight stack)
                |> List.sum

        totalHeight =
            sortedStacks
                |> List.map stackHeight
                |> List.sum

        midY =
            sumOfYsWeightedByHeight / totalHeight

        allReferences =
            sortedStacks
                |> List.map .references
                |> List.concat
    in
        { topY = midY + totalHeight / 2
        , bottomY = midY - totalHeight / 2
        , references = allReferences
        }

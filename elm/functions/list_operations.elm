module ListOperations exposing (..)

-- List operations in Elm

import Html exposing (text)


main =
    text (demo ())


demo () =
    let
        numbers =
            [ 1, 2, 3, 4, 5 ]

        -- List operations
        lengthResult =
            List.length numbers

        headResult =
            List.head numbers

        tailResult =
            List.tail numbers

        -- Map
        squared =
            List.map (\x -> x * x) numbers

        -- Filter
        evens =
            List.filter (\x -> modBy 2 x == 0) numbers

        -- Fold
        sum =
            List.foldl (+) 0 numbers

        -- Reverse
        reversed =
            List.reverse numbers

        -- Take and drop
        taken =
            List.take 3 numbers

        dropped =
            List.drop 2 numbers

        -- Range
        range =
            List.range 1 10

        -- List comprehension
        squares =
            List.map (\x -> x * x) (List.range 1 5)

        -- Any and all
        anyGreater =
            List.any (\x -> x > 4) numbers

        allPositive =
            List.all (\x -> x > 0) numbers
    in
    String.join "\n"
        [ "\n=== List Operations in Elm ==="
        , ""
        , "Numbers: " ++ Debug.toString numbers
        , "Length: " ++ Debug.toString lengthResult
        , "Head: " ++ Debug.toString headResult
        , "Tail: " ++ Debug.toString tailResult
        , ""
        , "Map (square): " ++ Debug.toString squared
        , "Filter (even): " ++ Debug.toString evens
        , ""
        , "Fold (sum): " ++ Debug.toString sum
        , "Reversed: " ++ Debug.toString reversed
        , ""
        , "Take 3: " ++ Debug.toString taken
        , "Drop 2: " ++ Debug.toString dropped
        , ""
        , "Range 1-10: " ++ Debug.toString range
        , "Squares: " ++ Debug.toString squares
        , ""
        , "Any > 4: " ++ Debug.toString anyGreater
        , "All > 0: " ++ Debug.toString allPositive
        ]

module Helpers exposing (..)

{-| Common helper functions when working with lists
-}


{-| remove element at given index from list
-}
dropFromList : Int -> List a -> List a
dropFromList index list =
    list
        |> List.indexedMap
            (\index_ hat ->
                if index == index_ then
                    Nothing
                else
                    Just hat
            )
        |> List.filterMap identity


{-| Get index of first element that satisfies the check
-}
findFirst : (a -> Bool) -> List a -> Maybe Int
findFirst check list =
    findFirstHelper check 0 list


findFirstHelper : (a -> Bool) -> Int -> List a -> Maybe Int
findFirstHelper check index list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            if check head then
                Just index
            else
                findFirstHelper check (index + 1) tail


{-| Update only the first element in the list that satisfies the check
-}
updateFirst : (a -> Bool) -> (a -> a) -> List a -> List a
updateFirst check transform list =
    let
        helper before list =
            case list of
                [] ->
                    List.reverse before

                head :: tail ->
                    if check head then
                        List.append
                            (List.reverse (transform head :: before))
                            tail
                    else
                        helper (head :: before) tail
    in
    helper [] list

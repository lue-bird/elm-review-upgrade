module List.LocalExtra exposing (firstJustMap, setUnionMap)

import Set exposing (Set)


setUnionMap : (a -> Set comparable) -> (List a -> Set comparable)
setUnionMap elementToSet =
    \list ->
        list
            |> List.foldl
                (\element soFar ->
                    Set.union soFar (element |> elementToSet)
                )
                Set.empty


firstJustMap : (a -> Maybe b) -> List a -> Maybe b
firstJustMap mapper nodes =
    case nodes of
        [] ->
            Nothing

        node :: rest ->
            case mapper node of
                Just value ->
                    Just value

                Nothing ->
                    firstJustMap mapper rest

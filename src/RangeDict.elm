module RangeDict exposing (RangeDict, any, empty, get, insert, mapFromList, member, remove, singleton, union)

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)


type RangeDict v
    = RangeDict (Dict ( ( Int, Int ), ( Int, Int ) ) v)


empty : RangeDict v_
empty =
    RangeDict Dict.empty


rangeToTuple : Range -> ( ( Int, Int ), ( Int, Int ) )
rangeToTuple range =
    ( ( range.start.row, range.start.column )
    , ( range.end.row, range.end.column )
    )


tupleToRange : ( ( Int, Int ), ( Int, Int ) ) -> Range
tupleToRange =
    \( ( startRow, startColumn ), ( endRow, endColumn ) ) ->
        { start = { row = startRow, column = startColumn }
        , end = { row = endRow, column = endColumn }
        }


singleton : Range -> v -> RangeDict v
singleton range value =
    RangeDict (Dict.singleton (rangeToTuple range) value)


{-| Indirect conversion from a list to key-value pairs to avoid successive List.map calls.
-}
mapFromList : (a -> ( Range, v )) -> List a -> RangeDict v
mapFromList toAssociation list =
    List.foldl
        (\element acc ->
            let
                ( range, v ) =
                    toAssociation element
            in
            Dict.insert (rangeToTuple range) v acc
        )
        Dict.empty
        list
        |> RangeDict


insert : Range -> v -> RangeDict v -> RangeDict v
insert range value (RangeDict rangeDict) =
    RangeDict (Dict.insert (rangeToTuple range) value rangeDict)


remove : Range -> RangeDict v -> RangeDict v
remove range (RangeDict rangeDict) =
    RangeDict (Dict.remove (rangeToTuple range) rangeDict)


get : Range -> RangeDict v -> Maybe v
get range (RangeDict rangeDict) =
    Dict.get (rangeToTuple range) rangeDict


member : Range -> RangeDict v_ -> Bool
member range (RangeDict rangeDict) =
    Dict.member (rangeToTuple range) rangeDict


any : (Range -> v -> Bool) -> RangeDict v -> Bool
any isFound rangeDict =
    foldl (\range value soFar -> soFar || isFound range value)
        False
        rangeDict


foldl : (Range -> v -> folded -> folded) -> folded -> RangeDict v -> folded
foldl reduce initialFolded (RangeDict rangeDict) =
    Dict.foldl (\tuple -> reduce (tuple |> tupleToRange)) initialFolded rangeDict


union : RangeDict v -> RangeDict v -> RangeDict v
union (RangeDict aRangeDict) (RangeDict bRangeDict) =
    RangeDict (Dict.union aRangeDict bRangeDict)

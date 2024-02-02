module RangeDict exposing (RangeDict, any, empty, get, insert, mapFromList, member, one, remove, union)

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)


type RangeDict value
    = RangeDict (Dict ( ( Int, Int ), ( Int, Int ) ) value)


empty : RangeDict v_
empty =
    RangeDict Dict.empty


rangeToTuple : Range -> ( ( Int, Int ), ( Int, Int ) )
rangeToTuple =
    \range ->
        ( ( range.start.row, range.start.column )
        , ( range.end.row, range.end.column )
        )


one : ( Range, value ) -> RangeDict value
one =
    \( range, value ) ->
        RangeDict (Dict.singleton (rangeToTuple range) value)


{-| Indirect conversion from a list to key-value pairs to avoid successive List.map calls.
-}
mapFromList : (element -> ( Range, value )) -> List element -> RangeDict value
mapFromList toAssociation =
    \list ->
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


insert : ( Range, value ) -> (RangeDict value -> RangeDict value)
insert ( range, value ) =
    \(RangeDict rangeDict) ->
        RangeDict (Dict.insert (rangeToTuple range) value rangeDict)


remove : Range -> (RangeDict value -> RangeDict value)
remove range =
    \(RangeDict rangeDict) ->
        RangeDict (Dict.remove (rangeToTuple range) rangeDict)


get : Range -> (RangeDict value -> Maybe value)
get range =
    \(RangeDict rangeDict) ->
        Dict.get (rangeToTuple range) rangeDict


member : Range -> (RangeDict value_ -> Bool)
member range =
    \(RangeDict rangeDict) ->
        Dict.member (rangeToTuple range) rangeDict


any : (( Range, value ) -> Bool) -> RangeDict value -> Bool
any isFound =
    \rangeDict ->
        foldl (\( range, value ) soFar -> soFar || isFound ( range, value ))
            False
            rangeDict


foldl : (( Range, value ) -> folded -> folded) -> folded -> RangeDict value -> folded
foldl reduce initialFolded =
    \(RangeDict rangeDict) ->
        rangeDict
            |> Dict.foldl (\tuple value -> reduce ( tuple |> tupleToRange, value )) initialFolded


tupleToRange : ( ( Int, Int ), ( Int, Int ) ) -> Range
tupleToRange =
    \( ( startRow, startColumn ), ( endRow, endColumn ) ) ->
        { start = { row = startRow, column = startColumn }
        , end = { row = endRow, column = endColumn }
        }


union : RangeDict v -> RangeDict v -> RangeDict v
union (RangeDict aRangeDict) (RangeDict bRangeDict) =
    RangeDict (Dict.union aRangeDict bRangeDict)

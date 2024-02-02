module Pattern.LocalExtra exposing (bindings, listBindings, nodeUsedModules, qualify, usedModules)

import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import List.LocalExtra
import ModuleName
import Qualification
import Set exposing (Set)


{-| Recursively find all bindings in a pattern.
-}
bindings : Pattern -> Set String
bindings pattern =
    -- IGNORE TCO
    case pattern of
        Elm.Syntax.Pattern.ListPattern patterns ->
            listBindings patterns

        Elm.Syntax.Pattern.TuplePattern patterns ->
            listBindings patterns

        Elm.Syntax.Pattern.RecordPattern patterns ->
            Set.fromList (List.map Elm.Syntax.Node.value patterns)

        Elm.Syntax.Pattern.NamedPattern _ patterns ->
            listBindings patterns

        Elm.Syntax.Pattern.UnConsPattern (Node _ headPattern) (Node _ tailPattern) ->
            Set.union (bindings tailPattern) (bindings headPattern)

        Elm.Syntax.Pattern.VarPattern variableName ->
            Set.singleton variableName

        Elm.Syntax.Pattern.AsPattern (Node _ pattern_) (Node _ variableName) ->
            Set.insert variableName (bindings pattern_)

        Elm.Syntax.Pattern.ParenthesizedPattern (Node _ inParens) ->
            bindings inParens

        Elm.Syntax.Pattern.AllPattern ->
            Set.empty

        Elm.Syntax.Pattern.UnitPattern ->
            Set.empty

        Elm.Syntax.Pattern.CharPattern _ ->
            Set.empty

        Elm.Syntax.Pattern.StringPattern _ ->
            Set.empty

        Elm.Syntax.Pattern.IntPattern _ ->
            Set.empty

        Elm.Syntax.Pattern.HexPattern _ ->
            Set.empty

        Elm.Syntax.Pattern.FloatPattern _ ->
            Set.empty


listBindings : List (Node Pattern) -> Set String
listBindings patterns =
    List.foldl
        (\(Node _ pattern) soFar -> Set.union soFar (pattern |> bindings))
        Set.empty
        patterns


qualify : Qualification.Context resources_ -> (Pattern -> Pattern)
qualify resources =
    \pattern ->
        pattern
            |> map
                (\innerPattern ->
                    case innerPattern of
                        Elm.Syntax.Pattern.NamedPattern fullyQualified arguments ->
                            Elm.Syntax.Pattern.NamedPattern
                                { name = fullyQualified.name
                                , moduleName =
                                    ( fullyQualified.moduleName |> ModuleName.fromSyntax, fullyQualified.name )
                                        |> Qualification.inContext resources
                                        |> Tuple.first
                                        |> ModuleName.toSyntax
                                }
                                arguments

                        otherPattern ->
                            otherPattern
                )


{-| Map it, then all its sub-patterns, all the way down
-}
map : (Pattern -> Pattern) -> (Pattern -> Pattern)
map patternChange =
    let
        step : Node Pattern -> Node Pattern
        step =
            Elm.Syntax.Node.map (\stepPattern -> stepPattern |> map patternChange)
    in
    -- IGNORE TCO
    \pattern ->
        case pattern |> patternChange of
            Elm.Syntax.Pattern.AllPattern ->
                Elm.Syntax.Pattern.AllPattern

            Elm.Syntax.Pattern.UnitPattern ->
                Elm.Syntax.Pattern.UnitPattern

            Elm.Syntax.Pattern.CharPattern char ->
                Elm.Syntax.Pattern.CharPattern char

            Elm.Syntax.Pattern.StringPattern string ->
                Elm.Syntax.Pattern.StringPattern string

            Elm.Syntax.Pattern.IntPattern int ->
                Elm.Syntax.Pattern.IntPattern int

            Elm.Syntax.Pattern.HexPattern int ->
                Elm.Syntax.Pattern.HexPattern int

            Elm.Syntax.Pattern.FloatPattern float ->
                Elm.Syntax.Pattern.FloatPattern float

            Elm.Syntax.Pattern.VarPattern name ->
                Elm.Syntax.Pattern.VarPattern name

            Elm.Syntax.Pattern.RecordPattern fieldNames ->
                Elm.Syntax.Pattern.RecordPattern fieldNames

            Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
                Elm.Syntax.Pattern.ParenthesizedPattern (inParens |> step)

            Elm.Syntax.Pattern.AsPattern aliased name ->
                Elm.Syntax.Pattern.AsPattern (aliased |> step) name

            Elm.Syntax.Pattern.UnConsPattern head tail ->
                Elm.Syntax.Pattern.UnConsPattern (head |> step) (tail |> step)

            Elm.Syntax.Pattern.TuplePattern parts ->
                Elm.Syntax.Pattern.TuplePattern (parts |> List.map step)

            Elm.Syntax.Pattern.ListPattern elements ->
                Elm.Syntax.Pattern.ListPattern (elements |> List.map step)

            Elm.Syntax.Pattern.NamedPattern qualified arguments ->
                Elm.Syntax.Pattern.NamedPattern qualified (arguments |> List.map step)


usedModules : Pattern -> Set String
usedModules =
    -- IGNORE TCO
    \pattern ->
        case pattern of
            Elm.Syntax.Pattern.AllPattern ->
                Set.empty

            Elm.Syntax.Pattern.UnitPattern ->
                Set.empty

            Elm.Syntax.Pattern.CharPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.StringPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.IntPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.HexPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.FloatPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.VarPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.RecordPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
                inParens |> nodeUsedModules

            Elm.Syntax.Pattern.AsPattern aliased _ ->
                aliased |> nodeUsedModules

            Elm.Syntax.Pattern.UnConsPattern head tail ->
                Set.union (tail |> nodeUsedModules) (head |> nodeUsedModules)

            Elm.Syntax.Pattern.TuplePattern parts ->
                parts |> List.LocalExtra.setUnionMap nodeUsedModules

            Elm.Syntax.Pattern.ListPattern elements ->
                elements |> List.LocalExtra.setUnionMap nodeUsedModules

            Elm.Syntax.Pattern.NamedPattern fullyQualified arguments ->
                arguments
                    |> List.LocalExtra.setUnionMap nodeUsedModules
                    |> Set.insert (fullyQualified.moduleName |> ModuleName.fromSyntax)


nodeUsedModules : Node Pattern -> Set String
nodeUsedModules =
    \(Node _ innerPattern) -> innerPattern |> usedModules

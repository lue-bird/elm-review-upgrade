module Type.LocalExtra exposing (nodeUsedModules, qualify, subs, usedModules)

import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation
import List.LocalExtra
import ModuleName
import Qualification
import Set exposing (Set)


qualify :
    Qualification.Context resources_
    -> (Elm.Syntax.TypeAnnotation.TypeAnnotation -> Elm.Syntax.TypeAnnotation.TypeAnnotation)
qualify resources =
    \type_ ->
        type_
            |> map
                (\innerType ->
                    case innerType of
                        Elm.Syntax.TypeAnnotation.Typed nameNode arguments ->
                            Elm.Syntax.TypeAnnotation.Typed
                                (nameNode
                                    |> Elm.Syntax.Node.map
                                        (\( moduleName, unqualifiedName ) ->
                                            ( ( moduleName |> ModuleName.fromSyntax, unqualifiedName )
                                                |> Qualification.inContext resources
                                                |> Tuple.first
                                                |> ModuleName.toSyntax
                                            , unqualifiedName
                                            )
                                        )
                                )
                                arguments

                        otherType ->
                            otherType
                )


{-| Map it, then all its sub-types, all the way down
-}
map :
    (Elm.Syntax.TypeAnnotation.TypeAnnotation -> Elm.Syntax.TypeAnnotation.TypeAnnotation)
    -> (Elm.Syntax.TypeAnnotation.TypeAnnotation -> Elm.Syntax.TypeAnnotation.TypeAnnotation)
map typeChange =
    let
        step : Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        step =
            Elm.Syntax.Node.map (\stepType -> stepType |> map typeChange)
    in
    -- IGNORE TCO
    \type_ ->
        case type_ |> typeChange of
            Elm.Syntax.TypeAnnotation.Unit ->
                Elm.Syntax.TypeAnnotation.Unit

            Elm.Syntax.TypeAnnotation.GenericType name ->
                Elm.Syntax.TypeAnnotation.GenericType name

            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input output ->
                Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation (input |> step) (output |> step)

            Elm.Syntax.TypeAnnotation.Tupled parts ->
                Elm.Syntax.TypeAnnotation.Tupled (parts |> List.map step)

            Elm.Syntax.TypeAnnotation.Record fields ->
                Elm.Syntax.TypeAnnotation.Record
                    (fields |> List.map (Elm.Syntax.Node.map (\( name, value ) -> ( name, value |> step ))))

            Elm.Syntax.TypeAnnotation.GenericRecord extended fields ->
                Elm.Syntax.TypeAnnotation.GenericRecord extended
                    (fields
                        |> Elm.Syntax.Node.map
                            (List.map (Elm.Syntax.Node.map (\( name, value ) -> ( name, value |> step ))))
                    )

            Elm.Syntax.TypeAnnotation.Typed nameNode arguments ->
                Elm.Syntax.TypeAnnotation.Typed nameNode (arguments |> List.map step)


nodeUsedModules : Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> Set String
nodeUsedModules =
    \(Node _ type_) -> type_ |> usedModules


usedModules : Elm.Syntax.TypeAnnotation.TypeAnnotation -> Set String
usedModules =
    \type_ ->
        case type_ of
            Elm.Syntax.TypeAnnotation.GenericType _ ->
                Set.empty

            Elm.Syntax.TypeAnnotation.Unit ->
                Set.empty

            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input output ->
                Set.union (input |> nodeUsedModules) (output |> nodeUsedModules)

            Elm.Syntax.TypeAnnotation.Tupled parts ->
                parts |> List.LocalExtra.setUnionMap nodeUsedModules

            Elm.Syntax.TypeAnnotation.Record fields ->
                fields |> List.LocalExtra.setUnionMap (\(Node _ ( _, fieldValue )) -> fieldValue |> nodeUsedModules)

            Elm.Syntax.TypeAnnotation.GenericRecord _ (Node _ fields) ->
                fields |> List.LocalExtra.setUnionMap (\(Node _ ( _, fieldValue )) -> fieldValue |> nodeUsedModules)

            Elm.Syntax.TypeAnnotation.Typed (Node _ ( moduleName, _ )) arguments ->
                arguments
                    |> List.LocalExtra.setUnionMap nodeUsedModules
                    |> Set.insert (moduleName |> ModuleName.fromSyntax)


{-| Get all immediate child types of that type
-}
subs : Elm.Syntax.TypeAnnotation.TypeAnnotation -> List (Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
subs =
    \type_ ->
        case type_ of
            Elm.Syntax.TypeAnnotation.Unit ->
                []

            Elm.Syntax.TypeAnnotation.GenericType _ ->
                []

            Elm.Syntax.TypeAnnotation.Typed _ typeArguments ->
                typeArguments

            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation a b ->
                [ a, b ]

            Elm.Syntax.TypeAnnotation.Tupled innerTypes ->
                innerTypes

            Elm.Syntax.TypeAnnotation.Record fields ->
                fields |> List.map (\(Node _ ( _, value )) -> value)

            Elm.Syntax.TypeAnnotation.GenericRecord _ (Node _ fields) ->
                fields |> List.map (\(Node _ ( _, value )) -> value)

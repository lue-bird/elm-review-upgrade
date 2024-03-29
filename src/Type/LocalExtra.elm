module Type.LocalExtra exposing (needsParens, nodeReferences, qualify, subs, usedModules)

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


references : Elm.Syntax.TypeAnnotation.TypeAnnotation -> Set ( String, String )
references =
    \type_ ->
        case type_ of
            Elm.Syntax.TypeAnnotation.GenericType _ ->
                Set.empty

            Elm.Syntax.TypeAnnotation.Unit ->
                Set.empty

            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input output ->
                Set.union (input |> nodeReferences) (output |> nodeReferences)

            Elm.Syntax.TypeAnnotation.Tupled parts ->
                parts |> List.LocalExtra.setUnionMap nodeReferences

            Elm.Syntax.TypeAnnotation.Record fields ->
                fields |> List.LocalExtra.setUnionMap (\(Node _ ( _, fieldValue )) -> fieldValue |> nodeReferences)

            Elm.Syntax.TypeAnnotation.GenericRecord _ (Node _ fields) ->
                fields |> List.LocalExtra.setUnionMap (\(Node _ ( _, fieldValue )) -> fieldValue |> nodeReferences)

            Elm.Syntax.TypeAnnotation.Typed (Node _ ( moduleName, unqualifiedName )) arguments ->
                arguments
                    |> List.LocalExtra.setUnionMap nodeReferences
                    |> Set.insert ( moduleName |> ModuleName.fromSyntax, unqualifiedName )


usedModules : Elm.Syntax.TypeAnnotation.TypeAnnotation -> Set String
usedModules =
    \type_ ->
        type_
            |> references
            |> Set.map (\( moduleName, _ ) -> moduleName)
            |> Set.remove ""


nodeReferences : Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> Set ( String, String )
nodeReferences =
    \(Node _ type_) -> type_ |> references


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


needsParens : Elm.Syntax.TypeAnnotation.TypeAnnotation -> Bool
needsParens =
    \type_ ->
        case type_ of
            Elm.Syntax.TypeAnnotation.Unit ->
                False

            Elm.Syntax.TypeAnnotation.GenericType _ ->
                False

            Elm.Syntax.TypeAnnotation.Tupled _ ->
                False

            Elm.Syntax.TypeAnnotation.Record _ ->
                False

            Elm.Syntax.TypeAnnotation.GenericRecord _ _ ->
                False

            Elm.Syntax.TypeAnnotation.Typed _ typeArguments ->
                case typeArguments of
                    [] ->
                        False

                    _ :: _ ->
                        True

            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation _ _ ->
                True

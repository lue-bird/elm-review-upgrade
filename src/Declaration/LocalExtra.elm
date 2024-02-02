module Declaration.LocalExtra exposing (listBindings)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Node exposing (Node(..))
import Set exposing (Set)


listBindings : List (Node Declaration) -> Set String
listBindings declarationList =
    declarationList
        |> List.map (\(Node _ declaration) -> bindings declaration)
        |> List.foldl (\bindings_ soFar -> Set.union soFar bindings_) Set.empty


bindings : Declaration -> Set String
bindings declaration =
    case declaration of
        Elm.Syntax.Declaration.CustomTypeDeclaration variantType ->
            variantType.constructors
                |> List.map (\(Node _ variant) -> Elm.Syntax.Node.value variant.name)
                |> Set.fromList

        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
            Set.singleton
                (functionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .name
                    |> Elm.Syntax.Node.value
                )

        _ ->
            Set.empty

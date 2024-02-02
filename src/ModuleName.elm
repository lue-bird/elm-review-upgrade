module ModuleName exposing (fromSyntax, toSyntax)

import Elm.Syntax.ModuleName


fromSyntax : Elm.Syntax.ModuleName.ModuleName -> String
fromSyntax syntaxModuleName =
    syntaxModuleName |> String.join "."


toSyntax : String -> Elm.Syntax.ModuleName.ModuleName
toSyntax =
    \moduleName ->
        case moduleName of
            "" ->
                []

            nonEmptyModuleName ->
                nonEmptyModuleName |> String.split "."

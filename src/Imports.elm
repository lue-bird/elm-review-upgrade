module Imports exposing (Exposed(..), Imports, implicit, insertSyntaxImports, isExposedFrom)

import Dict exposing (Dict)
import Elm.Syntax.Exposing
import Elm.Syntax.Import
import Elm.Syntax.Node exposing (Node(..))
import List.LocalExtra
import ModuleName
import Set exposing (Set)


type alias Imports =
    Dict
        String
        { alias : Maybe String
        , exposed : Exposed -- includes names of found variants
        }


type Exposed
    = ExposedAll
    | ExposedSome (Set String)


{-| From the `elm/core` readme:

>
> ### Default Imports

> The modules in this package are so common, that some of them are imported by default in all Elm files. So it is as if every Elm file starts with these imports:
>
>     import Basics exposing (..)
>     import List exposing (List, (::))
>     import Maybe exposing (Maybe(..))
>     import Result exposing (Result(..))
>     import String exposing (String)
>     import Char exposing (Char)
>     import Tuple
>     import Debug
>     import Platform exposing (Program)
>     import Platform.Cmd as Cmd exposing (Cmd)
>     import Platform.Sub as Sub exposing (Sub)

-}
implicit : Imports
implicit =
    [ ( "Basics", { alias = Nothing, exposed = ExposedAll } )
    , ( "List", { alias = Nothing, exposed = ExposedSome (Set.fromList [ "List", "(::)" ]) } )
    , ( "Maybe", { alias = Nothing, exposed = ExposedSome (Set.fromList [ "Maybe", "Just", "Nothing" ]) } )
    , ( "Result", { alias = Nothing, exposed = ExposedSome (Set.fromList [ "Result", "Ok", "Err" ]) } )
    , ( "String", { alias = Nothing, exposed = ExposedSome (Set.singleton "String") } )
    , ( "Char", { alias = Nothing, exposed = ExposedSome (Set.singleton "Char") } )
    , ( "Tuple", { alias = Nothing, exposed = ExposedSome Set.empty } )
    , ( "Debug", { alias = Nothing, exposed = ExposedSome Set.empty } )
    , ( "Platform", { alias = Nothing, exposed = ExposedSome (Set.singleton "Program") } )
    , ( "Platform.Cmd", { alias = Just "Cmd", exposed = ExposedSome (Set.singleton "Cmd") } )
    , ( "Platform.Sub", { alias = Just "Sub", exposed = ExposedSome (Set.singleton "Sub") } )
    ]
        |> Dict.fromList


{-| Merge a given new import with the existing import lookup.
This is strongly preferred over Dict.insert since the implicit default imports can be overridden
-}
insertSyntaxImports : List (Node Elm.Syntax.Import.Import) -> Imports -> Imports
insertSyntaxImports syntaxImports =
    \imports ->
        List.foldl
            (\(Node _ import_) importsSoFar ->
                let
                    importInfo : { moduleName : String, exposed : Exposed, alias : Maybe String }
                    importInfo =
                        importContext import_
                in
                importsSoFar
                    |> insert importInfo.moduleName { alias = importInfo.alias, exposed = importInfo.exposed }
            )
            imports
            syntaxImports


importContext : Elm.Syntax.Import.Import -> { moduleName : String, exposed : Exposed, alias : Maybe String }
importContext import_ =
    { moduleName = import_.moduleName |> Elm.Syntax.Node.value |> ModuleName.fromSyntax
    , alias = import_.moduleAlias |> Maybe.map (\(Node _ parts) -> parts |> ModuleName.fromSyntax)
    , exposed =
        case import_.exposingList of
            Nothing ->
                ExposedSome Set.empty

            Just (Node _ existingExposing) ->
                case existingExposing of
                    Elm.Syntax.Exposing.All _ ->
                        ExposedAll

                    Elm.Syntax.Exposing.Explicit exposes ->
                        ExposedSome
                            (exposes
                                |> List.map (\(Node _ expose) -> expose |> exposeName)
                                |> Set.fromList
                            )
    }


exposeName : Elm.Syntax.Exposing.TopLevelExpose -> String
exposeName topLevelExpose =
    case topLevelExpose of
        Elm.Syntax.Exposing.FunctionExpose exposeValueReferenceName ->
            exposeValueReferenceName

        Elm.Syntax.Exposing.TypeOrAliasExpose typeName ->
            typeName

        Elm.Syntax.Exposing.InfixExpose symbol ->
            symbol

        Elm.Syntax.Exposing.TypeExpose typeExpose ->
            typeExpose.name


insert : String -> { alias : Maybe String, exposed : Exposed } -> Imports -> Imports
insert moduleName importInfoToAdd imports =
    Dict.update moduleName
        (\existingImport ->
            let
                newImportInfo : { alias : Maybe String, exposed : Exposed }
                newImportInfo =
                    case existingImport of
                        Nothing ->
                            importInfoToAdd

                        Just import_ ->
                            { alias = List.LocalExtra.firstJustMap .alias [ import_, importInfoToAdd ]
                            , exposed = exposedMerge ( import_.exposed, importInfoToAdd.exposed )
                            }
            in
            Just newImportInfo
        )
        imports


exposedMerge : ( Exposed, Exposed ) -> Exposed
exposedMerge exposedTuple =
    case exposedTuple of
        ( ExposedAll, _ ) ->
            ExposedAll

        ( ExposedSome _, ExposedAll ) ->
            ExposedAll

        ( ExposedSome aSet, ExposedSome bSet ) ->
            ExposedSome (Set.union aSet bSet)


isExposedFrom : Exposed -> (String -> Bool)
isExposedFrom exposed =
    \potentialMember ->
        case exposed of
            ExposedAll ->
                True

            ExposedSome some ->
                some |> Set.member potentialMember

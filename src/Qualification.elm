module Qualification exposing (Context, defaultContext, inContext, isBindingInScope)

import Dict
import Imports exposing (Imports)
import RangeDict exposing (RangeDict)
import Set exposing (Set)


defaultContext : Context {}
defaultContext =
    { imports = Imports.implicit
    , localBindings = RangeDict.empty
    , moduleBindings = Set.empty
    }


type alias Context a =
    { a
        | imports : Imports
        , moduleBindings : Set String
        , localBindings : RangeDict (Set String)
    }


inContext : Context resources_ -> (( String, String ) -> ( String, String ))
inContext qualifyResources =
    \( moduleName, unqualifiedName ) ->
        let
            qualification : String
            qualification =
                case qualifyResources.imports |> Dict.get moduleName of
                    Nothing ->
                        moduleName

                    Just import_ ->
                        let
                            moduleImportedName : String
                            moduleImportedName =
                                import_.alias |> Maybe.withDefault moduleName
                        in
                        if not (unqualifiedName |> Imports.isExposedFrom import_.exposed) then
                            moduleImportedName

                        else
                            let
                                isShadowed : Bool
                                isShadowed =
                                    isBindingInScope qualifyResources unqualifiedName
                            in
                            if isShadowed then
                                moduleImportedName

                            else
                                ""
        in
        ( qualification, unqualifiedName )


isBindingInScope :
    { resources_
        | moduleBindings : Set String
        , localBindings : RangeDict (Set String)
    }
    -> String
    -> Bool
isBindingInScope resources binding =
    (resources.moduleBindings |> Set.member binding)
        || (resources.localBindings
                |> RangeDict.any (\( _, bindings ) -> bindings |> Set.member binding)
           )

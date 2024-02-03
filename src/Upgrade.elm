module Upgrade exposing
    ( rule
    , Upgrade
    , reference, application, call, pipeInto
    , typeReference, type_
    , batch
    , UpgradeSingle(..)
    )

{-| Reports when an outdated function/type can be replaced.

🔧 Running with `--fix` will automatically remove all the reported errors.

    config =
        [ Upgrade.rule
            [ Upgrade.reference { old = ( "MyUtil", "findMap" ), new = ( "List.Extra", "findMap" ) }
            , Upgrade.application
                { oldName = ( "Array.Extra", "apply" )
                , oldArgumentNames = [ "functions", "arguments" ]
                , oldArgumentsToNew =
                    \oldArguments ->
                        case oldArguments of
                            [ functions, arguments ] ->
                                Upgrade.call ( "Array.Extra", "andMap" )
                                    [ arguments, functions ]
                                    |> Just

                            _ ->
                                Nothing
                }
            ]
        ]

@docs rule
@docs Upgrade
@docs reference, application, call, pipeInto
@docs typeReference, type_
@docs batch


## safe internals

@docs UpgradeSingle

-}

import Declaration.LocalExtra
import Dict exposing (Dict)
import Elm.CodeGen
import Elm.Pretty
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation
import Expression.LocalExtra
import Imports exposing (Imports)
import List.LocalExtra
import ListFilled
import ModuleName
import Pattern.LocalExtra
import Pretty
import Qualification
import RangeDict exposing (RangeDict)
import Review.Fix
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule exposing (Error, Rule)
import Rope exposing (Rope)
import Set exposing (Set)
import Type.LocalExtra



-- upgrade


{-| Describes a bunch of transformations to your code. To create one:

  - [`Upgrade.reference`](#reference), [`Upgrade.application`](#application)
  - [`Upgrade.typeReference`](#reference), [`Upgrade.type_`](#application)

To group a few of them together, use [`Upgrade.batch`](#batch)

-}
type alias Upgrade =
    Rope UpgradeSingle


{-| An upgrade for a single function/value/type. A bunch of them are one [`Upgrade`](#Upgrade)
-}
type UpgradeSingle
    = Application
        { oldName : ( String, String )
        , oldArgumentNames : List String
        , oldArgumentsToNew :
            List Expression
            ->
                Maybe
                    ( { name : ( String, String )
                      , arguments : List Expression
                      }
                    , List
                        { name : ( String, String )
                        , arguments : List Expression
                        }
                    )
        }
    | Type
        { oldName : ( String, String )
        , oldArgumentsToNew :
            List Elm.Syntax.TypeAnnotation.TypeAnnotation
            -> Maybe Elm.Syntax.TypeAnnotation.TypeAnnotation
        }


{-| [`Upgrade`](#Upgrade) only the name of the value/function.
For example to replace every `MyUtil.findMap` with `List.Extra.findMap`:

    Upgrade.reference { old = "MyUtil", "findMap" ), new = ( "List.Extra", "findMap" ) }

-}
reference : { old : ( String, String ), new : ( String, String ) } -> Upgrade
reference nameChange =
    application
        { oldName = nameChange.old
        , oldArgumentNames = []
        , oldArgumentsToNew =
            \oldArguments -> Just (call nameChange.new oldArguments)
        }


{-| Flexible [`Upgrade`](#Upgrade) for a transformation from usage of a given function/value reference
to a [pipeline](#pipeInto) or a [call](#call).

For example to do describe the transformation

    Expect.true onFalseDescription actualBool
    --> Expect.equal True actualBool |> Expect.onFail onFalseDescription

as an [`Upgrade.application`](#application):

    Upgrade.application
        { oldName = ( "Expect", "true" )
        , oldArgumentNames = [ "onFalseDescription", "actualBool" ]
        , oldArgumentsToNew =
            \oldArguments ->
                case oldArguments of
                    [ onFalseDescriptionArgument, boolArgument ] ->
                        Upgrade.call ( "Expect", "equal" )
                            [ Elm.CodeGen.fqVal [ "Basics" ] "True"
                            , boolArgument
                            ]
                            |> Upgrade.pipeInto ( "Expect", "onFail" )
                                [ onFalseDescriptionArgument ]
                            |> Just

                    _ ->
                        Nothing
        }

Here's another example to change

    Array.Extra.call funs arguments
    --> Array.Extra.andMap arguments funs

as an [`Upgrade.application`](#application):

    Upgrade.application
        { oldName = ( "Array.Extra", "call" )
        , oldArgumentNames = [ "functions", "arguments" ]
        , oldArgumentsToNew =
            \oldArguments ->
                case oldArguments of
                    [ functionsArgument, argumentsArgument ] ->
                        Upgrade.application ( "Array.Extra", "andMap" )
                            [ argumentsArgument, functionsArgument ]
                            |> Just

                    _ ->
                        Nothing
        }

You can also use any expression as arguments to the functions in the pipeline.
To construct these, use [`elm-syntax-dsl`](https://dark.elm.dmy.fr/packages/the-sett/elm-syntax-dsl/latest/)
or [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) directly.

-}
application :
    { oldName : ( String, String )
    , oldArgumentNames : List String
    , oldArgumentsToNew :
        List Expression
        ->
            Maybe
                ( { name : ( String, String )
                  , arguments : List Expression
                  }
                , List
                    { name : ( String, String )
                    , arguments : List Expression
                    }
                )
    }
    -> Upgrade
application config =
    Rope.singleton (Application config)


{-| Flexible [`Upgrade`](#Upgrade) for a transformation of a given type constructor
to an equivalent type.

For example to do describe the transformation

    Endo from to
    --> (from -> to) -> from -> to

as a [`Upgrade.type_`](#type_):

    Upgrade.type_
        { oldName = ( "Endo", "Endo" )
        , oldArgumentsToNew =
            \oldArguments ->
                case oldArguments of
                    [ from, to ] ->
                        Elm.CodeGen.funAnn
                            (Elm.CodeGen.funAnn from to)
                            (Elm.CodeGen.funAnn from to)
                            |> Just

                    _ ->
                        Nothing
        }

You can use any types in the replacement type.
To construct these, use [`elm-syntax-dsl`](https://dark.elm.dmy.fr/packages/the-sett/elm-syntax-dsl/latest/)
or [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) directly.

-}
type_ :
    { oldName : ( String, String )
    , oldArgumentsToNew :
        List Elm.Syntax.TypeAnnotation.TypeAnnotation
        -> Maybe Elm.Syntax.TypeAnnotation.TypeAnnotation
    }
    -> Upgrade
type_ config =
    Rope.singleton (Type config)


{-| [`Upgrade`](#Upgrade) only the name of the type.
For example to replace every `Web.ProgramConfig` with `Web.Program.Config`:

    Upgrade.typeReference
        { old = "Web", "ProgramConfig" )
        , new = ( "Web.Program", "Config" )
        }

-}
typeReference : { old : ( String, String ), new : ( String, String ) } -> Upgrade
typeReference nameChange =
    type_
        { oldName = nameChange.old
        , oldArgumentsToNew =
            \oldArguments ->
                Elm.CodeGen.fqTyped
                    (nameChange.new |> Tuple.first |> ModuleName.toSyntax)
                    (nameChange.new |> Tuple.second)
                    oldArguments
                    |> Just
        }


{-| Construct an application as the transformed replacement value of an [`Upgrade.application`](Upgrade#application).
Use [`pipeInto`](#pipeInto) if you want to use its result as the input of a pipeline.
-}
call :
    ( String, String )
    -> List Expression
    ->
        ( { name : ( String, String )
          , arguments : List Expression
          }
        , List
            { name : ( String, String )
            , arguments : List Expression
            }
        )
call qualifiedName arguments =
    ( { name = qualifiedName, arguments = arguments }, [] )


{-| Extend the transformed value by `|> anotherFunction plus arguments`.
For example to get

    List.map mapper |> List.reverse

→

    Upgrade.call ( "List", "map" ) [ mapperArgument ]
        |> Upgrade.pipeInto ( "List", "reverse" ) []

-}
pipeInto :
    ( String, String )
    -> List Expression
    ->
        (( { name : ( String, String )
           , arguments : List Expression
           }
         , List
            { name : ( String, String )
            , arguments : List Expression
            }
         )
         ->
            ( { name : ( String, String )
              , arguments : List Expression
              }
            , List
                { name : ( String, String )
                , arguments : List Expression
                }
            )
        )
pipeInto qualifiedName argumentsExceptTheLastOne =
    \pipelineSoFar ->
        pipelineSoFar
            |> ListFilled.attach
                [ { name = qualifiedName, arguments = argumentsExceptTheLastOne } ]


{-| Group multiple individual [`Upgrade`](#Upgrade)s as one [`Upgrade`](#Upgrade).

    Upgrade.rule
        [ testVersion1To2
        , elmcraftCoreExtraVersion1To2
        , myInternalAPIChange
        ]

    testVersion1To2 : Upgrade
    testVersion1To2 =
        Upgrade.batch
            [ Upgrade.reference { old = ( "Fuzz", "tuple" ), new = ( "Fuzz", "pair" ) }
            , Upgrade.reference { old = ( "Fuzz", "tuple3" ), new = ( "Fuzz", "triple" ) }
            , ..etc..
            ]

    elmcraftCoreExtraVersion1To2 : Upgrade
    elmcraftCoreExtraVersion1To2 =
        Upgrade.batch [ ... ]

    myInternalAPIChange : Upgrade
    myInternalAPIChange =
        Upgrade.batch [ ... ]

Helps keep the upgrades a bit more tidy, less `List.concat`s and such.

-}
batch : List Upgrade -> Upgrade
batch =
    \upgradeList -> upgradeList |> Rope.fromList |> Rope.concat


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , moduleName : String
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , branchLocalBindings : RangeDict (Set String)
    , rangesToIgnore : RangeDict ()
    , extractSourceCode : Range -> String
    , imports : Imports
    , importRow : Int
    }


type alias ApplicationUpgradeResources =
    { lookupTable : ModuleNameLookupTable
    , imports : Imports
    , extractSourceCode : Range -> String
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , range : Range
    , referenceRange : Range
    , arguments : List (Node Expression)
    }


type alias TypeUpgradeResources =
    { lookupTable : ModuleNameLookupTable
    , imports : Imports
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , arguments : List (Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
    }


{-| The rule performing the given [`Upgrade`](#Upgrade)
-}
rule : List Upgrade -> Rule
rule upgrades =
    let
        upgrade : Upgrade
        upgrade =
            upgrades |> Rope.fromList |> Rope.concat

        applicationUpgradeReplacementsByOldName :
            Dict
                ( String, String )
                { oldArgumentCount : Int
                , toNew :
                    ApplicationUpgradeResources
                    -> Maybe { replacement : String, replacementDescription : String, usedModules : Set String }
                }
        applicationUpgradeReplacementsByOldName =
            upgrade |> applicationUpgradeReplacements

        typeUpgradeByOldName :
            Dict
                ( String, String )
                (TypeUpgradeResources -> Maybe { replacement : String, usedModules : Set String })
        typeUpgradeByOldName =
            upgrade |> typeUpgradeReplacements
    in
    Review.Rule.newModuleRuleSchemaUsingContextCreator "Upgrade" initialContext
        |> Review.Rule.providesFixesForModuleRule
        |> Review.Rule.withCommentsVisitor
            (\comments context ->
                case comments |> List.LocalExtra.firstJustMap (commentToModuleCommentRange context) of
                    Just moduleCommentRange ->
                        ( []
                        , { context | importRow = moduleCommentRange.end.row + 1 }
                        )

                    Nothing ->
                        ( [], context )
            )
        |> Review.Rule.withDeclarationListVisitor
            (\declarationList context -> ( [], declarationListVisitor declarationList context ))
        |> Review.Rule.withDeclarationEnterVisitor
            (\(Node _ declaration) context -> declarationVisitor declaration typeUpgradeByOldName context)
        |> Review.Rule.withExpressionEnterVisitor
            (\expressionNode context ->
                expressionNode
                    |> expressionVisitor
                        { application = applicationUpgradeReplacementsByOldName
                        , type_ = typeUpgradeByOldName
                        }
                        context
            )
        |> Review.Rule.withExpressionExitVisitor
            (\node context -> ( [], expressionExitVisitor node context ))
        |> Review.Rule.fromModuleRuleSchema


commentToModuleCommentRange : { resources_ | extractSourceCode : Range -> String } -> (Node String -> Maybe Range)
commentToModuleCommentRange resources =
    \(Node commentRange comment) ->
        if comment |> String.startsWith "{-|" then
            case
                { start = { row = commentRange.end.row + 1, column = 1 }
                , end = { row = commentRange.end.row + 1, column = 4 }
                }
                    |> resources.extractSourceCode
            of
                "port" ->
                    Nothing

                _ ->
                    Just commentRange

        else
            Nothing


initialContext : Review.Rule.ContextCreator () ModuleContext
initialContext =
    Review.Rule.initContextCreator
        (\lookupTable moduleName extractSourceCode fullAst () ->
            { lookupTable = lookupTable
            , moduleName = moduleName |> ModuleName.fromSyntax
            , imports =
                Imports.implicit |> Imports.insertSyntaxImports fullAst.imports
            , moduleBindings = Set.empty
            , localBindings = RangeDict.empty
            , branchLocalBindings = RangeDict.empty
            , rangesToIgnore = RangeDict.empty
            , extractSourceCode = extractSourceCode
            , importRow = 2
            }
        )
        |> Review.Rule.withModuleNameLookupTable
        |> Review.Rule.withModuleName
        |> Review.Rule.withSourceCodeExtractor
        |> Review.Rule.withFullAst



-- EXPRESSION VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ModuleContext
declarationListVisitor declarationList context =
    { context
        | moduleBindings = declarationList |> Declaration.LocalExtra.listBindings
    }


{-| Put a `ModuleName` and thing name together as a string.
If desired, call in combination with `Qualification.inContext`
-}
qualifiedToString : ( String, String ) -> String
qualifiedToString ( moduleName, unqualifiedName ) =
    case moduleName of
        "" ->
            unqualifiedName

        existingModuleName ->
            [ existingModuleName, ".", unqualifiedName ] |> String.concat


modulesToImportsString : Set String -> String
modulesToImportsString =
    \modulesToImport ->
        modulesToImport
            |> Set.remove ""
            |> Set.toList
            |> List.concatMap (\moduleName -> [ "import ", moduleName, "\n" ])
            |> String.concat


typeUpgradePerform :
    Dict
        ( String, String )
        (TypeUpgradeResources -> Maybe { replacement : String, usedModules : Set String })
    -> ModuleContext
    ->
        (Node Elm.Syntax.TypeAnnotation.TypeAnnotation
         -> List (Review.Rule.Error {})
        )
typeUpgradePerform upgradeByOldName context =
    -- IGNORE TCO
    \typeToUpgrade ->
        case typeToUpgrade of
            Node typeRange (Elm.Syntax.TypeAnnotation.Typed (Node referenceRange ( _, unqualifiedName )) arguments) ->
                case Review.ModuleNameLookupTable.moduleNameAt context.lookupTable referenceRange of
                    Nothing ->
                        []

                    Just moduleName ->
                        let
                            oldName : ( String, String )
                            oldName =
                                ( moduleName |> ModuleName.fromSyntax, unqualifiedName )
                        in
                        case upgradeByOldName |> Dict.get oldName of
                            Nothing ->
                                []

                            Just upgradeToPerform ->
                                let
                                    maybeUpgraded : Maybe { replacement : String, usedModules : Set String }
                                    maybeUpgraded =
                                        upgradeToPerform
                                            { lookupTable = context.lookupTable
                                            , moduleBindings = context.moduleBindings
                                            , localBindings = context.localBindings
                                            , imports = context.imports
                                            , arguments = arguments
                                            }
                                in
                                case maybeUpgraded of
                                    Nothing ->
                                        []

                                    Just upgraded ->
                                        [ Review.Rule.errorWithFix
                                            { message =
                                                (oldName
                                                    |> Qualification.inContext Qualification.defaultContext
                                                    |> qualifiedToString
                                                )
                                                    ++ " can be upgraded"
                                            , details =
                                                [ "I suggest applying the automatic fix, then cleaning it up in a way you like."
                                                ]
                                            }
                                            referenceRange
                                            [ Review.Fix.replaceRangeBy typeRange upgraded.replacement
                                            , Review.Fix.insertAt { row = context.importRow, column = 1 }
                                                (Set.diff upgraded.usedModules
                                                    (context.imports |> Dict.keys |> Set.fromList)
                                                    |> modulesToImportsString
                                                )
                                            ]
                                        ]

            Node _ otherType ->
                otherType
                    |> Type.LocalExtra.subs
                    |> List.concatMap
                        (typeUpgradePerform upgradeByOldName context)


declarationVisitor :
    Declaration
    ->
        Dict
            ( String, String )
            (TypeUpgradeResources -> Maybe { replacement : String, usedModules : Set String })
    -> ModuleContext
    -> ( List (Review.Rule.Error {}), ModuleContext )
declarationVisitor declaration typeUpgradeByOldName context =
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
            ( case functionDeclaration.signature of
                Nothing ->
                    []

                Just (Node _ signature) ->
                    signature.typeAnnotation
                        |> typeUpgradePerform typeUpgradeByOldName context
            , { context
                | rangesToIgnore = RangeDict.empty
                , localBindings =
                    RangeDict.one
                        ( functionDeclaration.declaration |> Elm.Syntax.Node.range
                        , functionDeclaration.declaration |> Elm.Syntax.Node.value |> .arguments |> Pattern.LocalExtra.listBindings
                        )
              }
            )

        Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
            ( typeAliasDeclaration.typeAnnotation
                |> typeUpgradePerform typeUpgradeByOldName context
            , context
            )

        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
            ( choiceTypeDeclaration.constructors
                |> List.concatMap
                    (\(Node _ variant) ->
                        variant.arguments
                            |> List.concatMap
                                (typeUpgradePerform typeUpgradeByOldName context)
                    )
            , context
            )

        _ ->
            ( [], context )


expressionVisitor :
    { application :
        Dict
            ( String, String )
            { oldArgumentCount : Int
            , toNew :
                ApplicationUpgradeResources
                -> Maybe { replacement : String, replacementDescription : String, usedModules : Set String }
            }
    , type_ :
        Dict
            ( String, String )
            (TypeUpgradeResources -> Maybe { replacement : String, usedModules : Set String })
    }
    -> ModuleContext
    ->
        (Node Expression
         -> ( List (Error {}), ModuleContext )
        )
expressionVisitor upgrade context =
    \expressionNode ->
        let
            expressionRange : Range
            expressionRange =
                expressionNode |> Elm.Syntax.Node.range
        in
        if RangeDict.any (\( ignoreRange, () ) -> ignoreRange |> rangeContainsLocation expressionRange.start) context.rangesToIgnore then
            ( [], context )

        else
            let
                expression : Expression
                expression =
                    expressionNode |> Elm.Syntax.Node.value

                withExpressionSurfaceBindings : RangeDict (Set String)
                withExpressionSurfaceBindings =
                    context.localBindings |> RangeDict.insert ( expressionRange, expression |> Expression.LocalExtra.surfaceBindings )

                withNewBranchLocalBindings : RangeDict (Set String)
                withNewBranchLocalBindings =
                    RangeDict.union
                        context.branchLocalBindings
                        (expression |> Expression.LocalExtra.branchLocalBindings)

                contextWithLocalBindings : ModuleContext
                contextWithLocalBindings =
                    case RangeDict.get expressionRange context.branchLocalBindings of
                        Nothing ->
                            { context
                                | localBindings = withExpressionSurfaceBindings
                                , branchLocalBindings = withNewBranchLocalBindings
                            }

                        Just currentBranchLocalBindings ->
                            { context
                                | localBindings =
                                    withExpressionSurfaceBindings |> RangeDict.insert ( expressionRange, currentBranchLocalBindings )
                                , branchLocalBindings =
                                    withNewBranchLocalBindings |> RangeDict.remove expressionRange
                            }
            in
            case expressionNode |> Expression.LocalExtra.toReferenceOrApplication of
                Just referenceOrApplication ->
                    case referenceOrApplication |> applicationUpgradePerform upgrade.application contextWithLocalBindings of
                        Nothing ->
                            ( [], contextWithLocalBindings )

                        Just successfulUpgrade ->
                            ( [ Review.Rule.errorWithFix
                                    { message =
                                        [ successfulUpgrade.name |> Qualification.inContext Qualification.defaultContext |> qualifiedToString
                                        , " can be upgraded to "
                                        , successfulUpgrade.replacementDescription
                                        ]
                                            |> String.concat
                                    , details =
                                        [ "I suggest applying the automatic fix, then cleaning it up in a way you like."
                                        ]
                                    }
                                    successfulUpgrade.referenceRange
                                    [ Review.Fix.replaceRangeBy
                                        successfulUpgrade.range
                                        successfulUpgrade.replacement
                                    , Review.Fix.insertAt { row = context.importRow, column = 1 }
                                        (Set.diff successfulUpgrade.usedModules
                                            (context.imports |> Dict.keys |> Set.fromList)
                                            |> modulesToImportsString
                                        )
                                    ]
                              ]
                            , { contextWithLocalBindings
                                | rangesToIgnore = context.rangesToIgnore |> RangeDict.insert ( successfulUpgrade.range, () )
                              }
                            )

                Nothing ->
                    ( case expression of
                        Elm.Syntax.Expression.LetExpression letIn ->
                            letIn.declarations
                                |> List.concatMap
                                    (\(Node _ letDeclaration) ->
                                        case letDeclaration of
                                            Elm.Syntax.Expression.LetDestructuring _ _ ->
                                                []

                                            Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                                                case letValueOrFunctionDeclaration.signature of
                                                    Nothing ->
                                                        []

                                                    Just (Node _ signature) ->
                                                        signature.typeAnnotation
                                                            |> typeUpgradePerform upgrade.type_ context
                                    )

                        _ ->
                            []
                    , contextWithLocalBindings
                    )


applicationUpgradePerform :
    Dict
        ( String, String )
        { oldArgumentCount : Int
        , toNew :
            ApplicationUpgradeResources
            -> Maybe { replacement : String, replacementDescription : String, usedModules : Set String }
        }
    -> ModuleContext
    ->
        ({ range : Range, name : String, referenceRange : Range, arguments : List (Node Expression) }
         ->
            Maybe
                { name : ( String, String )
                , referenceRange : Range
                , range : Range
                , replacement : String
                , replacementDescription : String
                , usedModules : Set String
                }
        )
applicationUpgradePerform upgrade context =
    \referenceOrApplication ->
        case Review.ModuleNameLookupTable.moduleNameAt context.lookupTable referenceOrApplication.referenceRange of
            Nothing ->
                Nothing

            Just moduleName ->
                case Dict.get ( moduleName |> ModuleName.fromSyntax, referenceOrApplication.name ) upgrade of
                    Nothing ->
                        Nothing

                    Just upgradeForName ->
                        let
                            range : Range
                            range =
                                case List.drop (upgradeForName.oldArgumentCount - 1) referenceOrApplication.arguments of
                                    lastExpectedArg :: _ :: _ ->
                                        -- extra arguments so we'll update the range to drop the extra ones
                                        { start = referenceOrApplication.referenceRange.start, end = (Elm.Syntax.Node.range lastExpectedArg).end }

                                    _ ->
                                        referenceOrApplication.range

                            arguments : List (Node Expression)
                            arguments =
                                -- drop the extra arguments
                                List.take upgradeForName.oldArgumentCount referenceOrApplication.arguments

                            upgradeResources : ApplicationUpgradeResources
                            upgradeResources =
                                { lookupTable = context.lookupTable
                                , extractSourceCode = context.extractSourceCode
                                , imports = context.imports
                                , moduleBindings = context.moduleBindings
                                , localBindings = context.localBindings
                                , range = range
                                , referenceRange = referenceOrApplication.referenceRange
                                , arguments = arguments
                                }
                        in
                        upgradeForName.toNew upgradeResources
                            |> Maybe.map
                                (\replacement ->
                                    { name = ( moduleName |> ModuleName.fromSyntax, referenceOrApplication.name )
                                    , referenceRange = referenceOrApplication.referenceRange
                                    , range = range
                                    , replacement = replacement.replacement
                                    , replacementDescription = replacement.replacementDescription
                                    , usedModules = replacement.usedModules
                                    }
                                )


rangeContainsLocation : Elm.Syntax.Range.Location -> Range -> Bool
rangeContainsLocation location =
    \range ->
        case ( Elm.Syntax.Range.compareLocations location range.start, Elm.Syntax.Range.compareLocations location range.end ) of
            ( LT, _ ) ->
                False

            ( _, GT ) ->
                False

            _ ->
                True


expressionExitVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionExitVisitor (Node expressionRange _) context =
    if RangeDict.member expressionRange context.rangesToIgnore then
        context

    else
        { context
            | localBindings =
                RangeDict.remove expressionRange context.localBindings
        }


applicationUpgradeReplacements :
    Upgrade
    ->
        Dict
            ( String, String )
            { oldArgumentCount : Int
            , toNew :
                ApplicationUpgradeResources
                -> Maybe { replacement : String, replacementDescription : String, usedModules : Set String }
            }
applicationUpgradeReplacements =
    \upgrade ->
        upgrade
            |> Rope.toList
            |> List.filterMap upgradeSingleToApplicationReplacement
            |> List.map
                (\singleReplacement ->
                    ( singleReplacement.oldName
                    , { oldArgumentCount = singleReplacement.oldArgumentCount
                      , toNew = singleReplacement.toNew
                      }
                    )
                )
            |> Dict.fromList


addIndentation : Int -> (String -> String)
addIndentation additionalIndentationLevel =
    \originalString ->
        originalString
            |> String.split "\n"
            |> List.map (\line -> String.repeat additionalIndentationLevel " " ++ line)
            |> String.join "\n"


upgradeSingleToApplicationReplacement :
    UpgradeSingle
    ->
        Maybe
            { oldName : ( String, String )
            , oldArgumentCount : Int
            , toNew :
                ApplicationUpgradeResources
                -> Maybe { replacement : String, replacementDescription : String, usedModules : Set String }
            }
upgradeSingleToApplicationReplacement =
    \upgradeSingle ->
        case upgradeSingle of
            Type _ ->
                Nothing

            Application applicationUpgrade ->
                { oldName = applicationUpgrade.oldName
                , oldArgumentCount = applicationUpgrade.oldArgumentNames |> List.length
                , toNew =
                    \upgradeInfo ->
                        let
                            missingArgumentNames : List String
                            missingArgumentNames =
                                applicationUpgrade.oldArgumentNames
                                    |> List.drop (upgradeInfo.arguments |> List.length)
                                    |> List.map (\missingArgument -> missingArgument |> disambiguateFromBindingsInScope upgradeInfo)

                            oldArguments : List Expression
                            oldArguments =
                                (upgradeInfo.arguments |> List.map Elm.Syntax.Node.value)
                                    ++ (missingArgumentNames |> List.map (\arg -> Elm.Syntax.Expression.FunctionOrValue [] arg))
                        in
                        case oldArguments |> applicationUpgrade.oldArgumentsToNew of
                            Nothing ->
                                Nothing

                            Just newPipeline ->
                                let
                                    usedModules : Set String
                                    usedModules =
                                        newPipeline
                                            |> ListFilled.toList
                                            |> List.concatMap (\inPipeline -> inPipeline.arguments)
                                            |> List.foldl (\arg soFar -> Set.union soFar (arg |> Expression.LocalExtra.usedModules))
                                                (newPipeline
                                                    |> ListFilled.toList
                                                    |> List.map (\r -> r.name |> Tuple.first)
                                                    |> Set.fromList
                                                )

                                    returnedString : String
                                    returnedString =
                                        newPipeline
                                            |> ListFilled.toList
                                            |> List.map
                                                (\referenceOrApplicationInPipeline ->
                                                    case referenceOrApplicationInPipeline.arguments of
                                                        [] ->
                                                            referenceOrApplicationInPipeline.name |> Qualification.inContext upgradeInfo |> qualifiedToString

                                                        argument0 :: arguments1Up ->
                                                            [ referenceOrApplicationInPipeline.name |> Qualification.inContext upgradeInfo |> qualifiedToString
                                                            , "\n"
                                                            , (argument0 :: arguments1Up)
                                                                |> List.map
                                                                    (\referenceOrApplicationInPipelineArgument ->
                                                                        let
                                                                            comesFromOld : Maybe Range
                                                                            comesFromOld =
                                                                                upgradeInfo.arguments
                                                                                    |> List.LocalExtra.firstJustMap
                                                                                        (\(Node oldArgumentRange oldArgumentExpression) ->
                                                                                            if oldArgumentExpression == referenceOrApplicationInPipelineArgument then
                                                                                                Just oldArgumentRange

                                                                                            else
                                                                                                Nothing
                                                                                        )

                                                                            newArgumentString : String
                                                                            newArgumentString =
                                                                                case comesFromOld of
                                                                                    Just oldArgumentRange ->
                                                                                        (String.repeat (oldArgumentRange.start.column - 1) " "
                                                                                            ++ upgradeInfo.extractSourceCode oldArgumentRange
                                                                                        )
                                                                                            |> removeIndentation

                                                                                    Nothing ->
                                                                                        referenceOrApplicationInPipelineArgument
                                                                                            |> Expression.LocalExtra.qualify upgradeInfo
                                                                                            |> Elm.Pretty.prettyExpression
                                                                                            |> Pretty.pretty 110
                                                                        in
                                                                        if referenceOrApplicationInPipelineArgument |> Expression.LocalExtra.needsParens then
                                                                            [ "(\n", newArgumentString, ")" ] |> String.concat

                                                                        else
                                                                            newArgumentString
                                                                    )
                                                                |> String.join "\n"
                                                                |> addIndentation 4
                                                            ]
                                                                |> String.concat
                                                )
                                            |> String.join " |>\n"
                                in
                                { replacement =
                                    { argumentNames = missingArgumentNames
                                    , returnedString = returnedString
                                    }
                                        |> toLambdaOrParenthesizedStringWithArgumentsMultiline
                                        |> String.split "\n"
                                        |> String.join ("\n" ++ String.repeat (upgradeInfo.range.start.column - 1) " ")
                                , replacementDescription =
                                    newPipeline
                                        |> ListFilled.toList
                                        |> List.map
                                            (\inPipeline ->
                                                inPipeline.name |> Qualification.inContext Qualification.defaultContext |> qualifiedToString
                                            )
                                        |> String.join ", then "
                                , usedModules = usedModules
                                }
                                    |> Just
                }
                    |> Just


toLambdaOrParenthesizedStringWithArgumentsMultiline : { argumentNames : List String, returnedString : String } -> String
toLambdaOrParenthesizedStringWithArgumentsMultiline lambdaOrParenthesized =
    case lambdaOrParenthesized.argumentNames of
        [] ->
            [ "(\n", lambdaOrParenthesized.returnedString, ")" ]
                |> String.concat

        argument0 :: arguments1Up ->
            [ "(\\"
            , (argument0 :: arguments1Up) |> String.join " "
            , " ->\n"
            , lambdaOrParenthesized.returnedString |> addIndentation 4
            , ")"
            ]
                |> String.concat


removeIndentation : String -> String
removeIndentation =
    \indentedString ->
        let
            indentedLines : List String
            indentedLines =
                indentedString |> String.lines
        in
        case indentedLines |> List.map lineIndentation |> List.minimum of
            Just smallestIndentationLevel ->
                indentedLines
                    |> List.map (\line -> line |> String.dropLeft smallestIndentationLevel)
                    |> String.join "\n"

            Nothing ->
                indentedString


lineIndentation : String -> Int
lineIndentation =
    \line ->
        case line |> String.uncons of
            Nothing ->
                0

            Just ( ' ', lineAfterSpace ) ->
                lineAfterSpace |> lineIndentation

            Just _ ->
                0


disambiguateFromBindingsInScope :
    { resources_
        | moduleBindings : Set String
        , localBindings : RangeDict (Set String)
    }
    -> (String -> String)
disambiguateFromBindingsInScope resources baseName =
    if
        (resources.moduleBindings |> Set.member baseName)
            || (resources.localBindings |> RangeDict.any (\( _, bindings ) -> bindings |> Set.member baseName))
    then
        disambiguateFromBindingsInScope resources (baseName ++ "_")

    else
        baseName


typeUpgradeReplacements :
    Upgrade
    ->
        Dict
            ( String, String )
            (TypeUpgradeResources
             -> Maybe { replacement : String, usedModules : Set String }
            )
typeUpgradeReplacements =
    \upgrade ->
        upgrade
            |> Rope.toList
            |> List.filterMap upgradeSingleToTypeReplacement
            |> List.map
                (\singleReplacement ->
                    ( singleReplacement.oldName, singleReplacement.toNew )
                )
            |> Dict.fromList


upgradeSingleToTypeReplacement :
    UpgradeSingle
    ->
        Maybe
            { oldName : ( String, String )
            , toNew :
                TypeUpgradeResources
                -> Maybe { replacement : String, usedModules : Set String }
            }
upgradeSingleToTypeReplacement upgradeSingle =
    case upgradeSingle of
        Application _ ->
            Nothing

        Type typeUpgrade ->
            { oldName = typeUpgrade.oldName
            , toNew =
                \upgradeResources ->
                    case typeUpgrade.oldArgumentsToNew (upgradeResources.arguments |> List.map Elm.Syntax.Node.value) of
                        Nothing ->
                            Nothing

                        Just new ->
                            { usedModules = new |> Type.LocalExtra.usedModules
                            , replacement =
                                new
                                    |> Type.LocalExtra.qualify upgradeResources
                                    |> Elm.Pretty.prettyTypeAnnotation
                                    |> Pretty.pretty 1000
                            }
                                |> Just
            }
                |> Just

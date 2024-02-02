module Upgrade exposing
    ( rule
    , Upgrade, UpgradeSingle, reference, application
    , call, pipeInto
    )

{-| Reports when an expression can be simplified.

🔧 Running with `--fix` will automatically remove all the reported errors.

    config =
        [ Upgrade.rule
            [ Upgrade.reference { old = ( "MyUtil", "findMap" ), new = ( "List.Extra", "findMap" ) }
            ]
        ]

@docs rule
@docs Upgrade, UpgradeSingle, reference, application
@docs call, pipeInto


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-upgrade/example --rules Upgrade
```

-}

import Dict exposing (Dict)
import Elm.Pretty
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Exposing
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation
import Pretty
import RangeDict exposing (RangeDict)
import Review.Fix
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule exposing (Error, Rule)
import Rope exposing (Rope)
import Set exposing (Set)



-- upgrade


{-| A bunch of [`UpgradeSingle`](#UpgradeSingle)s to transform your code
-}
type alias Upgrade =
    Rope UpgradeSingle


{-| An upgrade for a single function/value. A bunch of them are one [`Upgrade`](#Upgrade)
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
                        Upgrade.application
                            { name = ( "Expect", "equal" )
                            , arguments =
                                [ Elm.Syntax.Expression.FunctionOrValue [ "Basics" ] "True"
                                , boolArgument
                                ]
                            }
                            |> Upgrade.pipeInto
                                { name = ( "Expect", "onFail" )
                                , arguments = [ onFalseDescriptionArgument ]
                                }
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
            |> listFilledAttach
                [ { name = qualifiedName, arguments = argumentsExceptTheLastOne } ]



-- rule


listFilledHead : ListFilled a -> a
listFilledHead ( head, _ ) =
    head


listFilledTail : ListFilled a -> List a
listFilledTail ( _, tail ) =
    tail


listFilledAttach : List a -> (ListFilled a -> ListFilled a)
listFilledAttach attachment =
    \baseListFilled ->
        ( baseListFilled |> listFilledHead
        , (baseListFilled |> listFilledTail) ++ attachment
        )


listFirstJustMap : (a -> Maybe b) -> List a -> Maybe b
listFirstJustMap mapper nodes =
    case nodes of
        [] ->
            Nothing

        node :: rest ->
            case mapper node of
                Just value ->
                    Just value

                Nothing ->
                    listFirstJustMap mapper rest


{-| Rule to upgrade Elm code.
-}
rule : List Upgrade -> Rule
rule upgrades =
    let
        upgradeReplacementsByOldName :
            Dict
                ( String, String )
                { oldArgumentCount : Int
                , toNew :
                    UpgradeResources
                    -> Maybe { replacement : String, replacementDescription : String, usedModules : Set String }
                }
        upgradeReplacementsByOldName =
            upgrades
                |> Rope.fromList
                |> Rope.concat
                |> upgradeReplacements
    in
    Review.Rule.newModuleRuleSchemaUsingContextCreator "Upgrade" initialContext
        |> Review.Rule.providesFixesForModuleRule
        |> Review.Rule.withCommentsVisitor
            (\comments context ->
                case comments |> listFirstJustMap (commentToModuleCommentRange context) of
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
            (\(Node _ declaration) context -> ( [], declarationVisitor declaration context ))
        |> Review.Rule.withExpressionEnterVisitor
            (\expressionNode context ->
                expressionNode |> expressionVisitor upgradeReplacementsByOldName context
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
implicitImports : ImportLookup
implicitImports =
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


fromSyntaxModuleName : Elm.Syntax.ModuleName.ModuleName -> String
fromSyntaxModuleName moduleName =
    String.join "." moduleName


initialContext : Review.Rule.ContextCreator () ModuleContext
initialContext =
    Review.Rule.initContextCreator
        (\lookupTable moduleName extractSourceCode fullAst () ->
            { lookupTable = lookupTable
            , moduleName = moduleName |> fromSyntaxModuleName
            , importLookup =
                List.foldl
                    (\(Node _ import_) importLookup ->
                        let
                            importInfo : { moduleName : String, exposed : Exposed, alias : Maybe String }
                            importInfo =
                                importContext import_
                        in
                        insertImport importInfo.moduleName { alias = importInfo.alias, exposed = importInfo.exposed } importLookup
                    )
                    implicitImports
                    fullAst.imports
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


importContext : Import -> { moduleName : String, exposed : Exposed, alias : Maybe String }
importContext import_ =
    { moduleName = import_.moduleName |> Elm.Syntax.Node.value |> fromSyntaxModuleName
    , alias = import_.moduleAlias |> Maybe.map (\(Node _ parts) -> parts |> fromSyntaxModuleName)
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



-- EXPRESSION VISITOR


{-| Merge a given new import with an existing import lookup.
This is strongly preferred over Dict.insert since the implicit default imports can be overridden
-}
insertImport : String -> { alias : Maybe String, exposed : Exposed } -> ImportLookup -> ImportLookup
insertImport moduleName importInfoToAdd importLookup =
    Dict.update moduleName
        (\existingImport ->
            let
                newImportInfo : { alias : Maybe String, exposed : Exposed }
                newImportInfo =
                    case existingImport of
                        Nothing ->
                            importInfoToAdd

                        Just import_ ->
                            { alias = listFirstJustMap .alias [ import_, importInfoToAdd ]
                            , exposed = exposedMerge ( import_.exposed, importInfoToAdd.exposed )
                            }
            in
            Just newImportInfo
        )
        importLookup


exposedMerge : ( Exposed, Exposed ) -> Exposed
exposedMerge exposedTuple =
    case exposedTuple of
        ( ExposedAll, _ ) ->
            ExposedAll

        ( ExposedSome _, ExposedAll ) ->
            ExposedAll

        ( ExposedSome aSet, ExposedSome bSet ) ->
            ExposedSome (Set.union aSet bSet)


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ModuleContext
declarationListVisitor declarationList context =
    { context
        | moduleBindings = declarationList |> declarationListBindings
    }


declarationListBindings : List (Node Declaration) -> Set String
declarationListBindings declarationList =
    declarationList
        |> List.map (\(Node _ declaration) -> declarationBindings declaration)
        |> List.foldl (\bindings soFar -> Set.union soFar bindings) Set.empty


declarationBindings : Declaration -> Set String
declarationBindings declaration =
    case declaration of
        Elm.Syntax.Declaration.CustomTypeDeclaration variantType ->
            variantType.constructors
                |> List.map (\(Node _ variant) -> Elm.Syntax.Node.value variant.name)
                |> Set.fromList

        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
            Set.singleton
                (Elm.Syntax.Node.value (Elm.Syntax.Node.value functionDeclaration.declaration).name)

        _ ->
            Set.empty


{-| Recursively find all bindings in a pattern.
-}
patternBindings : Pattern -> Set String
patternBindings pattern =
    -- IGNORE TCO
    case pattern of
        Elm.Syntax.Pattern.ListPattern patterns ->
            patternListBindings patterns

        Elm.Syntax.Pattern.TuplePattern patterns ->
            patternListBindings patterns

        Elm.Syntax.Pattern.RecordPattern patterns ->
            Set.fromList (List.map Elm.Syntax.Node.value patterns)

        Elm.Syntax.Pattern.NamedPattern _ patterns ->
            patternListBindings patterns

        Elm.Syntax.Pattern.UnConsPattern (Node _ headPattern) (Node _ tailPattern) ->
            Set.union (patternBindings tailPattern) (patternBindings headPattern)

        Elm.Syntax.Pattern.VarPattern variableName ->
            Set.singleton variableName

        Elm.Syntax.Pattern.AsPattern (Node _ pattern_) (Node _ variableName) ->
            Set.insert variableName (patternBindings pattern_)

        Elm.Syntax.Pattern.ParenthesizedPattern (Node _ inParens) ->
            patternBindings inParens

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


patternListBindings : List (Node Pattern) -> Set String
patternListBindings patterns =
    List.foldl
        (\(Node _ pattern) soFar -> Set.union soFar (patternBindings pattern))
        Set.empty
        patterns


declarationVisitor : Declaration -> ModuleContext -> ModuleContext
declarationVisitor declarationNode context =
    case declarationNode of
        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
            { context
                | rangesToIgnore = RangeDict.empty
                , localBindings =
                    RangeDict.one
                        ( functionDeclaration.declaration |> Elm.Syntax.Node.range
                        , functionDeclaration.declaration |> Elm.Syntax.Node.value |> .arguments |> patternListBindings
                        )
            }

        _ ->
            context


defaultQualifyResources : QualifyResources {}
defaultQualifyResources =
    { importLookup = implicitImports
    , localBindings = RangeDict.empty
    , moduleBindings = Set.empty
    }


{-| Put a `ModuleName` and thing name together as a string.
If desired, call in combination with `qualify`
-}
qualifiedToString : ( String, String ) -> String
qualifiedToString ( moduleName, unqualifiedName ) =
    case moduleName of
        "" ->
            unqualifiedName

        existingModuleName ->
            [ existingModuleName, ".", unqualifiedName ] |> String.concat


qualify : QualifyResources resources_ -> (( String, String ) -> ( String, String ))
qualify qualifyResources =
    \( moduleName, unqualifiedName ) ->
        let
            qualification : String
            qualification =
                case qualifyResources.importLookup |> Dict.get moduleName of
                    Nothing ->
                        moduleName

                    Just import_ ->
                        let
                            moduleImportedName : String
                            moduleImportedName =
                                import_.alias |> Maybe.withDefault moduleName
                        in
                        if not (isExposedFrom import_.exposed unqualifiedName) then
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


isExposedFrom : Exposed -> (String -> Bool)
isExposedFrom exposed =
    \potentialMember ->
        case exposed of
            ExposedAll ->
                True

            ExposedSome some ->
                some |> Set.member potentialMember


isBindingInScope :
    { resources_
        | moduleBindings : Set String
        , localBindings : RangeDict (Set String)
    }
    -> String
    -> Bool
isBindingInScope resources binding =
    Set.member binding resources.moduleBindings
        || RangeDict.any (\( _, bindings ) -> Set.member binding bindings) resources.localBindings


expressionVisitor :
    Dict
        ( String, String )
        { oldArgumentCount : Int
        , toNew :
            UpgradeResources
            -> Maybe { replacement : String, replacementDescription : String, usedModules : Set String }
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
                    context.localBindings |> RangeDict.insert ( expressionRange, expressionSurfaceBindings expression )

                withNewBranchLocalBindings : RangeDict (Set String)
                withNewBranchLocalBindings =
                    RangeDict.union (expressionBranchLocalBindings expression)
                        context.branchLocalBindings

                contextWithInferredConstantsAndLocalBindings : ModuleContext
                contextWithInferredConstantsAndLocalBindings =
                    case RangeDict.get expressionRange context.branchLocalBindings of
                        Nothing ->
                            { context
                                | localBindings = withExpressionSurfaceBindings
                                , branchLocalBindings =
                                    withNewBranchLocalBindings
                            }

                        Just currentBranchLocalBindings ->
                            { context
                                | localBindings =
                                    withExpressionSurfaceBindings |> RangeDict.insert ( expressionRange, currentBranchLocalBindings )
                                , branchLocalBindings =
                                    withNewBranchLocalBindings |> RangeDict.remove expressionRange
                            }

                upgradePerformed :
                    Maybe
                        { name : ( String, String )
                        , referenceRange : Range
                        , range : Range
                        , replacement : String
                        , replacementDescription : String
                        , usedModules : Set String
                        }
                upgradePerformed =
                    expressionNode |> expressionUpgradePerform upgrade contextWithInferredConstantsAndLocalBindings
            in
            case upgradePerformed of
                Nothing ->
                    ( [], contextWithInferredConstantsAndLocalBindings )

                Just successfulUpgrade ->
                    ( [ Review.Rule.errorWithFix
                            { message =
                                [ successfulUpgrade.name |> qualify defaultQualifyResources |> qualifiedToString
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
                            , let
                                modulesToImport : Set String
                                modulesToImport =
                                    Set.diff successfulUpgrade.usedModules
                                        (context.importLookup |> Dict.keys |> Set.fromList)
                              in
                              Review.Fix.insertAt { row = context.importRow, column = 1 }
                                (modulesToImport
                                    |> Set.remove ""
                                    |> Set.toList
                                    |> List.concatMap (\moduleName -> [ "import ", moduleName, "\n" ])
                                    |> String.concat
                                )
                            ]
                      ]
                    , { contextWithInferredConstantsAndLocalBindings
                        | rangesToIgnore = context.rangesToIgnore |> RangeDict.insert ( successfulUpgrade.range, () )
                      }
                    )


{-| Whenever you add ranges on expression enter, the same ranges should be removed on expression exit.
Having one function finding unique ranges and a function for extracting bindings there ensures said consistency.

An alternative approach would be to use some kind of tree structure
with parent and sub ranges and bindings as leaves (maybe a "trie", tho I've not seen one as an elm package).

Removing all bindings for an expression's range on leave would then be trivial

-}
expressionSurfaceBindings : Expression -> Set String
expressionSurfaceBindings expression =
    case expression of
        Elm.Syntax.Expression.LambdaExpression lambda ->
            patternListBindings lambda.args

        Elm.Syntax.Expression.LetExpression letBlock ->
            letDeclarationListBindings letBlock.declarations

        _ ->
            Set.empty


letDeclarationListBindings : List (Node Elm.Syntax.Expression.LetDeclaration) -> Set String
letDeclarationListBindings letDeclarationList =
    letDeclarationList
        |> List.map
            (\(Node _ declaration) -> letDeclarationBindings declaration)
        |> List.foldl (\bindings soFar -> Set.union soFar bindings) Set.empty


letDeclarationBindings : Elm.Syntax.Expression.LetDeclaration -> Set String
letDeclarationBindings letDeclaration =
    case letDeclaration of
        Elm.Syntax.Expression.LetFunction fun ->
            Set.singleton
                (fun.declaration |> Elm.Syntax.Node.value |> .name |> Elm.Syntax.Node.value)

        Elm.Syntax.Expression.LetDestructuring (Node _ pattern) _ ->
            patternBindings pattern


expressionBranchLocalBindings : Expression -> RangeDict (Set String)
expressionBranchLocalBindings expression =
    case expression of
        Elm.Syntax.Expression.CaseExpression caseBlock ->
            RangeDict.mapFromList
                (\( Node _ pattern, Node resultRange _ ) ->
                    ( resultRange
                    , patternBindings pattern
                    )
                )
                caseBlock.cases

        Elm.Syntax.Expression.LetExpression letBlock ->
            List.foldl
                (\(Node _ letDeclaration) soFar ->
                    case letDeclaration of
                        Elm.Syntax.Expression.LetFunction letFunctionOrValueDeclaration ->
                            soFar
                                |> RangeDict.insert
                                    ( letFunctionOrValueDeclaration.declaration |> Elm.Syntax.Node.value |> .expression |> Elm.Syntax.Node.range
                                    , patternListBindings
                                        (Elm.Syntax.Node.value letFunctionOrValueDeclaration.declaration).arguments
                                    )

                        _ ->
                            soFar
                )
                RangeDict.empty
                letBlock.declarations

        _ ->
            RangeDict.empty


expressionUpgradePerform :
    Dict
        ( String, String )
        { oldArgumentCount : Int
        , toNew :
            UpgradeResources
            -> Maybe { replacement : String, replacementDescription : String, usedModules : Set String }
        }
    -> ModuleContext
    ->
        (Node Expression
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
expressionUpgradePerform upgrade context =
    \expressionNode ->
        case expressionNode |> toReferenceOrApplication of
            Nothing ->
                Nothing

            Just referenceOrApplication ->
                case Review.ModuleNameLookupTable.moduleNameAt context.lookupTable referenceOrApplication.referenceRange of
                    Nothing ->
                        Nothing

                    Just moduleName ->
                        case Dict.get ( moduleName |> fromSyntaxModuleName, referenceOrApplication.name ) upgrade of
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
                                                expressionNode |> Elm.Syntax.Node.range

                                    arguments : List (Node Expression)
                                    arguments =
                                        -- drop the extra arguments
                                        List.take upgradeForName.oldArgumentCount referenceOrApplication.arguments

                                    upgradeResources : UpgradeResources
                                    upgradeResources =
                                        { lookupTable = context.lookupTable
                                        , extractSourceCode = context.extractSourceCode
                                        , importLookup = context.importLookup
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
                                            { name = ( moduleName |> fromSyntaxModuleName, referenceOrApplication.name )
                                            , referenceRange = referenceOrApplication.referenceRange
                                            , range = range
                                            , replacement = replacement.replacement
                                            , replacementDescription = replacement.replacementDescription
                                            , usedModules = replacement.usedModules
                                            }
                                        )


listFilledToList : ListFilled a -> List a
listFilledToList =
    \( head, tail ) -> head :: tail


{-| Parses either a value reference
or a function reference with or without arguments.
-}
toReferenceOrApplication :
    Node Expression
    ->
        Maybe
            { range : Range
            , name : String
            , referenceRange : Range
            , arguments : List (Node Expression)
            }
toReferenceOrApplication baseNode =
    let
        step :
            { arguments : ListFilled (Node Expression), fed : Node Expression }
            -> Maybe { range : Range, referenceRange : Range, name : String, arguments : List (Node Expression) }
        step layer =
            layer.fed
                |> toReferenceOrApplication
                |> Maybe.map
                    (\fed ->
                        { range = baseNode |> Elm.Syntax.Node.range
                        , referenceRange = fed.referenceRange
                        , name = fed.name
                        , arguments = fed.arguments ++ (layer.arguments |> listFilledToList)
                        }
                    )
    in
    case baseNode |> removeParens of
        Node referenceRange (Elm.Syntax.Expression.FunctionOrValue _ unqualifiedName) ->
            Just
                { range = baseNode |> Elm.Syntax.Node.range
                , referenceRange = referenceRange
                , name = unqualifiedName
                , arguments = []
                }

        Node _ (Elm.Syntax.Expression.Application (fed :: argument0 :: argument1Up)) ->
            step
                { fed = fed
                , arguments = ( argument0, argument1Up )
                }

        Node _ (Elm.Syntax.Expression.OperatorApplication "|>" _ argument0 fed) ->
            step
                { fed = fed
                , arguments = ( argument0, [] )
                }

        Node _ (Elm.Syntax.Expression.OperatorApplication "<|" _ fed argument0) ->
            step
                { fed = fed
                , arguments = ( argument0, [] )
                }

        _ ->
            Nothing


{-| Keep removing parens from the outside until we have something different from a `ParenthesizedExpression`
-}
removeParens : Node Expression -> Node Expression
removeParens expressionNode =
    case expressionNode |> Elm.Syntax.Node.value of
        Elm.Syntax.Expression.ParenthesizedExpression expressionInsideOnePairOfParensNode ->
            removeParens expressionInsideOnePairOfParensNode

        _ ->
            expressionNode


expressionExitVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionExitVisitor (Node expressionRange _) context =
    if RangeDict.member expressionRange context.rangesToIgnore then
        context

    else
        { context
            | localBindings =
                RangeDict.remove expressionRange context.localBindings
        }


upgradeReplacements :
    Upgrade
    ->
        Dict
            ( String, String )
            { oldArgumentCount : Int
            , toNew :
                UpgradeResources
                -> Maybe { replacement : String, replacementDescription : String, usedModules : Set String }
            }
upgradeReplacements upgrade =
    upgrade
        |> Rope.toList
        |> List.map
            (\upgradeSingle ->
                let
                    singleReplacement :
                        { oldName : ( String, String )
                        , oldArgumentCount : Int
                        , toNew :
                            UpgradeResources
                            -> Maybe { replacement : String, replacementDescription : String, usedModules : Set String }
                        }
                    singleReplacement =
                        upgradeSingle |> upgradeSingleReplacement
                in
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


upgradeSingleReplacement :
    UpgradeSingle
    ->
        { oldName : ( String, String )
        , oldArgumentCount : Int
        , toNew :
            UpgradeResources
            -> Maybe { replacement : String, replacementDescription : String, usedModules : Set String }
        }
upgradeSingleReplacement =
    \upgradeSingle ->
        case upgradeSingle of
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
                                            |> listFilledToList
                                            |> List.concatMap (\inPipeline -> inPipeline.arguments)
                                            |> List.foldl (\arg soFar -> Set.union soFar (arg |> expressionUsedModules))
                                                (newPipeline
                                                    |> listFilledToList
                                                    |> List.map (\r -> r.name |> Tuple.first)
                                                    |> Set.fromList
                                                )

                                    returnedString : String
                                    returnedString =
                                        newPipeline
                                            |> listFilledToList
                                            |> List.map
                                                (\referenceOrApplicationInPipeline ->
                                                    case referenceOrApplicationInPipeline.arguments of
                                                        [] ->
                                                            referenceOrApplicationInPipeline.name |> qualify upgradeInfo |> qualifiedToString

                                                        argument0 :: arguments1Up ->
                                                            [ referenceOrApplicationInPipeline.name |> qualify upgradeInfo |> qualifiedToString
                                                            , "\n"
                                                            , (argument0 :: arguments1Up)
                                                                |> List.map
                                                                    (\referenceOrApplicationInPipelineArgument ->
                                                                        let
                                                                            comesFromOld : Maybe Range
                                                                            comesFromOld =
                                                                                upgradeInfo.arguments
                                                                                    |> listFirstJustMap
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
                                                                                            |> expressionQualify upgradeInfo
                                                                                            |> Elm.Pretty.prettyExpression
                                                                                            |> Pretty.pretty 110
                                                                        in
                                                                        if referenceOrApplicationInPipelineArgument |> expressionNeedsParens then
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
                                        |> listFilledToList
                                        |> List.map
                                            (\inPipeline ->
                                                inPipeline.name |> qualify defaultQualifyResources |> qualifiedToString
                                            )
                                        |> String.join ", then "
                                , usedModules = usedModules
                                }
                                    |> Just
                }


toSyntaxModuleName : String -> Elm.Syntax.ModuleName.ModuleName
toSyntaxModuleName =
    \moduleName ->
        case moduleName of
            "" ->
                []

            nonEmptyModuleName ->
                nonEmptyModuleName |> String.split "."


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


expressionNeedsParens : Expression -> Bool
expressionNeedsParens expr =
    case expr of
        Elm.Syntax.Expression.Application _ ->
            True

        Elm.Syntax.Expression.OperatorApplication _ _ _ _ ->
            True

        Elm.Syntax.Expression.IfBlock _ _ _ ->
            True

        Elm.Syntax.Expression.Negation _ ->
            True

        Elm.Syntax.Expression.LetExpression _ ->
            True

        Elm.Syntax.Expression.CaseExpression _ ->
            True

        Elm.Syntax.Expression.LambdaExpression _ ->
            True

        Elm.Syntax.Expression.UnitExpr ->
            False

        Elm.Syntax.Expression.CharLiteral _ ->
            False

        Elm.Syntax.Expression.Integer _ ->
            False

        Elm.Syntax.Expression.Hex _ ->
            False

        Elm.Syntax.Expression.Floatable _ ->
            False

        Elm.Syntax.Expression.Literal _ ->
            False

        Elm.Syntax.Expression.GLSLExpression _ ->
            False

        Elm.Syntax.Expression.PrefixOperator _ ->
            False

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            False

        Elm.Syntax.Expression.RecordAccess _ _ ->
            False

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            False

        Elm.Syntax.Expression.ParenthesizedExpression _ ->
            False

        Elm.Syntax.Expression.TupledExpression _ ->
            False

        Elm.Syntax.Expression.ListExpr _ ->
            False

        Elm.Syntax.Expression.RecordExpr _ ->
            False

        Elm.Syntax.Expression.RecordUpdateExpression _ _ ->
            False

        -- IMPOSSIBLE --
        Elm.Syntax.Expression.Operator _ ->
            False


expressionQualify : QualifyResources resources_ -> (Expression -> Expression)
expressionQualify resources =
    \expression ->
        expression
            |> expressionMap
                (\innerExpression ->
                    case innerExpression of
                        Elm.Syntax.Expression.FunctionOrValue qualification unqualifiedName ->
                            Elm.Syntax.Expression.FunctionOrValue
                                (( qualification |> fromSyntaxModuleName, unqualifiedName )
                                    |> qualify resources
                                    |> Tuple.first
                                    |> toSyntaxModuleName
                                )
                                unqualifiedName

                        Elm.Syntax.Expression.LambdaExpression lambda ->
                            Elm.Syntax.Expression.LambdaExpression
                                { args = lambda.args |> List.map (Elm.Syntax.Node.map (patternQualify resources))
                                , expression = lambda.expression
                                }

                        Elm.Syntax.Expression.CaseExpression caseOf ->
                            Elm.Syntax.Expression.CaseExpression
                                { expression = caseOf.expression
                                , cases =
                                    caseOf.cases
                                        |> List.map
                                            (\( patternNode, expressionNode ) ->
                                                ( patternNode |> Elm.Syntax.Node.map (patternQualify resources)
                                                , expressionNode
                                                )
                                            )
                                }

                        Elm.Syntax.Expression.LetExpression letIn ->
                            Elm.Syntax.Expression.LetExpression
                                { expression = letIn.expression
                                , declarations =
                                    letIn.declarations
                                        |> List.map
                                            (Elm.Syntax.Node.map
                                                (\letDeclaration ->
                                                    case letDeclaration of
                                                        Elm.Syntax.Expression.LetDestructuring patternNode expressionNode ->
                                                            Elm.Syntax.Expression.LetDestructuring
                                                                (patternNode |> Elm.Syntax.Node.map (patternQualify resources))
                                                                expressionNode

                                                        Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                                                            Elm.Syntax.Expression.LetFunction
                                                                { documentation = letValueOrFunctionDeclaration.documentation
                                                                , signature =
                                                                    case letValueOrFunctionDeclaration.signature of
                                                                        Nothing ->
                                                                            Nothing

                                                                        Just signatureNode ->
                                                                            signatureNode
                                                                                |> Elm.Syntax.Node.map
                                                                                    (\signature ->
                                                                                        { name = signature.name
                                                                                        , typeAnnotation =
                                                                                            signature.typeAnnotation
                                                                                                |> Elm.Syntax.Node.map (typeQualify resources)
                                                                                        }
                                                                                    )
                                                                                |> Just
                                                                , declaration =
                                                                    letValueOrFunctionDeclaration.declaration
                                                                        |> Elm.Syntax.Node.map
                                                                            (\declaration ->
                                                                                { expression = declaration.expression
                                                                                , name = declaration.name
                                                                                , arguments =
                                                                                    declaration.arguments
                                                                                        |> List.map (Elm.Syntax.Node.map (patternQualify resources))
                                                                                }
                                                                            )
                                                                }
                                                )
                                            )
                                }

                        otherExpression ->
                            otherExpression
                )


patternQualify : QualifyResources resources_ -> (Pattern -> Pattern)
patternQualify resources =
    \pattern ->
        pattern
            |> patternMap
                (\innerPattern ->
                    case innerPattern of
                        Elm.Syntax.Pattern.NamedPattern fullyQualified arguments ->
                            Elm.Syntax.Pattern.NamedPattern
                                { name = fullyQualified.name
                                , moduleName =
                                    ( fullyQualified.moduleName |> fromSyntaxModuleName, fullyQualified.name )
                                        |> qualify resources
                                        |> Tuple.first
                                        |> toSyntaxModuleName
                                }
                                arguments

                        otherPattern ->
                            otherPattern
                )


{-| Map it, then all its sub-expressions, all the way down
-}
patternMap : (Pattern -> Pattern) -> (Pattern -> Pattern)
patternMap patternChange =
    let
        step : Node Pattern -> Node Pattern
        step =
            Elm.Syntax.Node.map (\stepPattern -> stepPattern |> patternMap patternChange)
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


typeQualify :
    QualifyResources resources_
    -> (Elm.Syntax.TypeAnnotation.TypeAnnotation -> Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeQualify resources =
    \type_ ->
        type_
            |> typeMap
                (\innerType ->
                    case innerType of
                        Elm.Syntax.TypeAnnotation.Typed nameNode arguments ->
                            Elm.Syntax.TypeAnnotation.Typed
                                (nameNode
                                    |> Elm.Syntax.Node.map
                                        (\( moduleName, unqualifiedName ) ->
                                            ( ( moduleName |> fromSyntaxModuleName, unqualifiedName )
                                                |> qualify resources
                                                |> Tuple.first
                                                |> toSyntaxModuleName
                                            , unqualifiedName
                                            )
                                        )
                                )
                                arguments

                        otherType ->
                            otherType
                )


{-| Map it, then all its sub-expressions, all the way down
-}
typeMap :
    (Elm.Syntax.TypeAnnotation.TypeAnnotation -> Elm.Syntax.TypeAnnotation.TypeAnnotation)
    -> (Elm.Syntax.TypeAnnotation.TypeAnnotation -> Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeMap typeChange =
    let
        step : Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        step =
            Elm.Syntax.Node.map (\stepType -> stepType |> typeMap typeChange)
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


{-| Map it, then all its sub-expressions, all the way down
-}
expressionMap : (Expression -> Expression) -> (Expression -> Expression)
expressionMap expressionChange =
    -- IGNORE TCO
    \expression ->
        let
            step : Node Expression -> Node Expression
            step =
                Elm.Syntax.Node.map (\stepExpression -> stepExpression |> expressionMap expressionChange)
        in
        case expression |> expressionChange of
            Elm.Syntax.Expression.LetExpression letBlock ->
                Elm.Syntax.Expression.LetExpression
                    { expression = letBlock.expression |> step
                    , declarations =
                        letBlock.declarations
                            |> List.map
                                (Elm.Syntax.Node.map
                                    (\letDeclaration ->
                                        case letDeclaration of
                                            Elm.Syntax.Expression.LetFunction letFunction ->
                                                Elm.Syntax.Expression.LetFunction
                                                    { letFunction
                                                        | declaration =
                                                            letFunction.declaration
                                                                |> Elm.Syntax.Node.map (\fun -> { fun | expression = fun.expression |> step })
                                                    }

                                            Elm.Syntax.Expression.LetDestructuring pattern expression_ ->
                                                Elm.Syntax.Expression.LetDestructuring pattern (expression_ |> step)
                                    )
                                )
                    }

            Elm.Syntax.Expression.ListExpr expressions ->
                Elm.Syntax.Expression.ListExpr (expressions |> List.map step)

            Elm.Syntax.Expression.TupledExpression expressions ->
                Elm.Syntax.Expression.TupledExpression (expressions |> List.map step)

            Elm.Syntax.Expression.RecordExpr fields ->
                Elm.Syntax.Expression.RecordExpr (fields |> List.map (Elm.Syntax.Node.map (Tuple.mapSecond step)))

            Elm.Syntax.Expression.RecordUpdateExpression recordVariable setters ->
                Elm.Syntax.Expression.RecordUpdateExpression recordVariable
                    (setters |> List.map (Elm.Syntax.Node.map (Tuple.mapSecond step)))

            Elm.Syntax.Expression.RecordAccess recordToAccess fieldName ->
                Elm.Syntax.Expression.RecordAccess (recordToAccess |> step) fieldName

            Elm.Syntax.Expression.Application applicationElements ->
                Elm.Syntax.Expression.Application (applicationElements |> List.map step)

            Elm.Syntax.Expression.CaseExpression caseBlock ->
                Elm.Syntax.Expression.CaseExpression
                    { expression = caseBlock.expression
                    , cases = caseBlock.cases |> List.map (Tuple.mapSecond step)
                    }

            Elm.Syntax.Expression.OperatorApplication symbol direction left right ->
                Elm.Syntax.Expression.OperatorApplication symbol direction (left |> step) (right |> step)

            Elm.Syntax.Expression.IfBlock condition then_ else_ ->
                Elm.Syntax.Expression.IfBlock (condition |> step) (then_ |> step) (else_ |> step)

            Elm.Syntax.Expression.LambdaExpression lambda ->
                Elm.Syntax.Expression.LambdaExpression { lambda | expression = lambda.expression |> step }

            Elm.Syntax.Expression.ParenthesizedExpression expressionInParens ->
                Elm.Syntax.Expression.ParenthesizedExpression (expressionInParens |> step)

            Elm.Syntax.Expression.Negation expressionInNegation ->
                Elm.Syntax.Expression.Negation (expressionInNegation |> step)

            Elm.Syntax.Expression.UnitExpr ->
                Elm.Syntax.Expression.UnitExpr

            Elm.Syntax.Expression.Integer int ->
                Elm.Syntax.Expression.Integer int

            Elm.Syntax.Expression.Hex int ->
                Elm.Syntax.Expression.Hex int

            Elm.Syntax.Expression.Floatable float ->
                Elm.Syntax.Expression.Floatable float

            Elm.Syntax.Expression.Literal string ->
                Elm.Syntax.Expression.Literal string

            Elm.Syntax.Expression.CharLiteral char ->
                Elm.Syntax.Expression.CharLiteral char

            Elm.Syntax.Expression.GLSLExpression glsl ->
                Elm.Syntax.Expression.GLSLExpression glsl

            Elm.Syntax.Expression.RecordAccessFunction fieldName ->
                Elm.Syntax.Expression.RecordAccessFunction fieldName

            Elm.Syntax.Expression.FunctionOrValue qualification unqualifiedName ->
                Elm.Syntax.Expression.FunctionOrValue qualification unqualifiedName

            Elm.Syntax.Expression.Operator symbol ->
                Elm.Syntax.Expression.Operator symbol

            Elm.Syntax.Expression.PrefixOperator symbol ->
                Elm.Syntax.Expression.PrefixOperator symbol


listSetUnionMap : (a -> Set comparable) -> (List a -> Set comparable)
listSetUnionMap elementToSet =
    \list ->
        list
            |> List.foldl
                (\element soFar ->
                    Set.union soFar (element |> elementToSet)
                )
                Set.empty


patternUsedModules : Pattern -> Set String
patternUsedModules =
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
                inParens |> patternNodeUsedModules

            Elm.Syntax.Pattern.AsPattern aliased _ ->
                aliased |> patternNodeUsedModules

            Elm.Syntax.Pattern.UnConsPattern head tail ->
                Set.union (tail |> patternNodeUsedModules) (head |> patternNodeUsedModules)

            Elm.Syntax.Pattern.TuplePattern parts ->
                parts |> listSetUnionMap patternNodeUsedModules

            Elm.Syntax.Pattern.ListPattern elements ->
                elements |> listSetUnionMap patternNodeUsedModules

            Elm.Syntax.Pattern.NamedPattern fullyQualified arguments ->
                arguments
                    |> listSetUnionMap patternNodeUsedModules
                    |> Set.insert (fullyQualified.moduleName |> fromSyntaxModuleName)


typeNodeUsedModules : Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> Set String
typeNodeUsedModules =
    \(Node _ type_) -> type_ |> typeUsedModules


expressionUsedModules : Expression -> Set String
expressionUsedModules =
    -- IGNORE TCO
    \expression ->
        Set.union
            (expression
                |> subExpressions
                |> listSetUnionMap
                    (\(Node _ innerExpression) ->
                        innerExpression |> expressionUsedModules
                    )
            )
            (case expression of
                Elm.Syntax.Expression.FunctionOrValue qualification _ ->
                    qualification |> fromSyntaxModuleName |> Set.singleton

                Elm.Syntax.Expression.LambdaExpression lambda ->
                    lambda.args |> listSetUnionMap (\(Node _ pattern) -> pattern |> patternUsedModules)

                Elm.Syntax.Expression.CaseExpression caseOf ->
                    caseOf.cases
                        |> listSetUnionMap
                            (\( Node _ pattern, _ ) -> pattern |> patternUsedModules)

                Elm.Syntax.Expression.LetExpression letIn ->
                    letIn.declarations
                        |> listSetUnionMap
                            (\(Node _ letDeclaration) ->
                                case letDeclaration of
                                    Elm.Syntax.Expression.LetDestructuring (Node _ pattern) _ ->
                                        pattern |> patternUsedModules

                                    Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                                        Set.union
                                            (case letValueOrFunctionDeclaration.signature of
                                                Nothing ->
                                                    Set.empty

                                                Just (Node _ signature) ->
                                                    signature.typeAnnotation
                                                        |> typeNodeUsedModules
                                            )
                                            (letValueOrFunctionDeclaration.declaration
                                                |> Elm.Syntax.Node.value
                                                |> .arguments
                                                |> listSetUnionMap (\(Node _ pattern) -> pattern |> patternUsedModules)
                                            )
                            )

                _ ->
                    Set.empty
            )


{-| Get all immediate child expressions of an expression
-}
subExpressions : Expression -> List (Node Expression)
subExpressions expression =
    case expression of
        Elm.Syntax.Expression.LetExpression letBlock ->
            letBlock.expression
                :: (letBlock.declarations
                        |> List.map
                            (\(Node _ letDeclaration) ->
                                case letDeclaration of
                                    Elm.Syntax.Expression.LetFunction letFunction ->
                                        letFunction.declaration |> Elm.Syntax.Node.value |> .expression

                                    Elm.Syntax.Expression.LetDestructuring _ expression_ ->
                                        expression_
                            )
                   )

        Elm.Syntax.Expression.ListExpr expressions ->
            expressions

        Elm.Syntax.Expression.TupledExpression expressions ->
            expressions

        Elm.Syntax.Expression.RecordExpr fields ->
            fields |> List.map (\(Node _ ( _, value )) -> value)

        Elm.Syntax.Expression.RecordUpdateExpression _ setters ->
            setters |> List.map (\(Node _ ( _, newValue )) -> newValue)

        Elm.Syntax.Expression.RecordAccess recordToAccess _ ->
            [ recordToAccess ]

        Elm.Syntax.Expression.Application applicationElements ->
            applicationElements

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            caseBlock.expression
                :: (caseBlock.cases |> List.map (\( _, caseExpression ) -> caseExpression))

        Elm.Syntax.Expression.OperatorApplication _ _ e1 e2 ->
            [ e1, e2 ]

        Elm.Syntax.Expression.IfBlock condition then_ else_ ->
            [ condition, then_, else_ ]

        Elm.Syntax.Expression.LambdaExpression lambda ->
            [ lambda.expression ]

        Elm.Syntax.Expression.ParenthesizedExpression expressionInParens ->
            [ expressionInParens ]

        Elm.Syntax.Expression.Negation expressionInNegation ->
            [ expressionInNegation ]

        Elm.Syntax.Expression.UnitExpr ->
            []

        Elm.Syntax.Expression.Integer _ ->
            []

        Elm.Syntax.Expression.Hex _ ->
            []

        Elm.Syntax.Expression.Floatable _ ->
            []

        Elm.Syntax.Expression.Literal _ ->
            []

        Elm.Syntax.Expression.CharLiteral _ ->
            []

        Elm.Syntax.Expression.GLSLExpression _ ->
            []

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            []

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            []

        Elm.Syntax.Expression.Operator _ ->
            []

        Elm.Syntax.Expression.PrefixOperator _ ->
            []


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , moduleName : String
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , branchLocalBindings : RangeDict (Set String)
    , rangesToIgnore : RangeDict ()
    , extractSourceCode : Range -> String
    , importLookup : ImportLookup
    , importRow : Int
    }


type alias ImportLookup =
    Dict
        String
        { alias : Maybe String
        , exposed : Exposed -- includes names of found variants
        }


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



-- ListFilled


type alias QualifyResources a =
    { a
        | importLookup : ImportLookup
        , moduleBindings : Set String
        , localBindings : RangeDict (Set String)
    }


type Exposed
    = ExposedAll
    | ExposedSome (Set String)


type alias UpgradeResources =
    { lookupTable : ModuleNameLookupTable
    , importLookup : ImportLookup
    , extractSourceCode : Range -> String
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , range : Range
    , referenceRange : Range
    , arguments : List (Node Expression)
    }


patternNodeUsedModules : Node Pattern -> Set String
patternNodeUsedModules =
    \(Node _ innerPattern) -> innerPattern |> patternUsedModules


typeUsedModules : Elm.Syntax.TypeAnnotation.TypeAnnotation -> Set String
typeUsedModules =
    \type_ ->
        case type_ of
            Elm.Syntax.TypeAnnotation.GenericType _ ->
                Set.empty

            Elm.Syntax.TypeAnnotation.Unit ->
                Set.empty

            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input output ->
                Set.union (input |> typeNodeUsedModules) (output |> typeNodeUsedModules)

            Elm.Syntax.TypeAnnotation.Tupled parts ->
                parts |> listSetUnionMap typeNodeUsedModules

            Elm.Syntax.TypeAnnotation.Record fields ->
                fields |> listSetUnionMap (\(Node _ ( _, fieldValue )) -> fieldValue |> typeNodeUsedModules)

            Elm.Syntax.TypeAnnotation.GenericRecord _ (Node _ fields) ->
                fields |> listSetUnionMap (\(Node _ ( _, fieldValue )) -> fieldValue |> typeNodeUsedModules)

            Elm.Syntax.TypeAnnotation.Typed (Node _ ( moduleName, _ )) arguments ->
                arguments
                    |> listSetUnionMap typeNodeUsedModules
                    |> Set.insert (moduleName |> fromSyntaxModuleName)



-- List


type alias ListFilled element =
    ( element, List element )

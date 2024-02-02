module Expression.LocalExtra exposing (branchLocalBindings, needsParens, qualify, surfaceBindings, toReferenceOrApplication, usedModules)

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import List.LocalExtra
import ListFilled exposing (ListFilled)
import ModuleName
import Pattern.LocalExtra
import Qualification
import RangeDict exposing (RangeDict)
import Set exposing (Set)
import Type.LocalExtra


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
                        , arguments = fed.arguments ++ (layer.arguments |> ListFilled.toList)
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


needsParens : Expression -> Bool
needsParens =
    \expression ->
        case expression of
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


branchLocalBindings : Expression -> RangeDict (Set String)
branchLocalBindings expression =
    case expression of
        Elm.Syntax.Expression.CaseExpression caseBlock ->
            RangeDict.mapFromList
                (\( Node _ pattern, Node resultRange _ ) ->
                    ( resultRange
                    , pattern |> Pattern.LocalExtra.bindings
                    )
                )
                caseBlock.cases

        Elm.Syntax.Expression.LetExpression letBlock ->
            letBlock.declarations
                |> List.foldl
                    (\(Node _ letDeclaration) soFar ->
                        case letDeclaration of
                            Elm.Syntax.Expression.LetFunction letFunctionOrValueDeclaration ->
                                soFar
                                    |> RangeDict.insert
                                        ( letFunctionOrValueDeclaration.declaration |> Elm.Syntax.Node.value |> .expression |> Elm.Syntax.Node.range
                                        , letFunctionOrValueDeclaration.declaration
                                            |> Elm.Syntax.Node.value
                                            |> .arguments
                                            |> Pattern.LocalExtra.listBindings
                                        )

                            _ ->
                                soFar
                    )
                    RangeDict.empty

        _ ->
            RangeDict.empty


{-| Whenever you add ranges on expression enter, the same ranges should be removed on expression exit.
Having one function finding unique ranges and a function for extracting bindings there ensures said consistency.
-}
surfaceBindings : Expression -> Set String
surfaceBindings expression =
    case expression of
        Elm.Syntax.Expression.LambdaExpression lambda ->
            lambda.args |> Pattern.LocalExtra.listBindings

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
            pattern |> Pattern.LocalExtra.bindings


qualify : Qualification.Context resources_ -> (Expression -> Expression)
qualify resources =
    \expression ->
        expression
            |> map
                (\innerExpression ->
                    case innerExpression of
                        Elm.Syntax.Expression.FunctionOrValue qualification unqualifiedName ->
                            Elm.Syntax.Expression.FunctionOrValue
                                (( qualification |> ModuleName.fromSyntax, unqualifiedName )
                                    |> Qualification.inContext resources
                                    |> Tuple.first
                                    |> ModuleName.toSyntax
                                )
                                unqualifiedName

                        Elm.Syntax.Expression.LambdaExpression lambda ->
                            Elm.Syntax.Expression.LambdaExpression
                                { args = lambda.args |> List.map (Elm.Syntax.Node.map (Pattern.LocalExtra.qualify resources))
                                , expression = lambda.expression
                                }

                        Elm.Syntax.Expression.CaseExpression caseOf ->
                            Elm.Syntax.Expression.CaseExpression
                                { expression = caseOf.expression
                                , cases =
                                    caseOf.cases
                                        |> List.map
                                            (\( patternNode, expressionNode ) ->
                                                ( patternNode |> Elm.Syntax.Node.map (Pattern.LocalExtra.qualify resources)
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
                                                                (patternNode |> Elm.Syntax.Node.map (Pattern.LocalExtra.qualify resources))
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
                                                                                                |> Elm.Syntax.Node.map (Type.LocalExtra.qualify resources)
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
                                                                                        |> List.map (Elm.Syntax.Node.map (Pattern.LocalExtra.qualify resources))
                                                                                }
                                                                            )
                                                                }
                                                )
                                            )
                                }

                        otherExpression ->
                            otherExpression
                )


{-| Map it, then all its sub-expressions, all the way down
-}
map : (Expression -> Expression) -> (Expression -> Expression)
map expressionChange =
    -- IGNORE TCO
    \expression ->
        let
            step : Node Expression -> Node Expression
            step =
                Elm.Syntax.Node.map (\stepExpression -> stepExpression |> map expressionChange)
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


usedModules : Expression -> Set String
usedModules =
    -- IGNORE TCO
    \expression ->
        Set.union
            (expression
                |> subs
                |> List.LocalExtra.setUnionMap
                    (\(Node _ innerExpression) ->
                        innerExpression |> usedModules
                    )
            )
            (case expression of
                Elm.Syntax.Expression.FunctionOrValue qualification _ ->
                    qualification |> ModuleName.fromSyntax |> Set.singleton

                Elm.Syntax.Expression.LambdaExpression lambda ->
                    lambda.args |> List.LocalExtra.setUnionMap Pattern.LocalExtra.nodeUsedModules

                Elm.Syntax.Expression.CaseExpression caseOf ->
                    caseOf.cases
                        |> List.LocalExtra.setUnionMap
                            (\( Node _ pattern, _ ) -> pattern |> Pattern.LocalExtra.usedModules)

                Elm.Syntax.Expression.LetExpression letIn ->
                    letIn.declarations
                        |> List.LocalExtra.setUnionMap
                            (\(Node _ letDeclaration) ->
                                case letDeclaration of
                                    Elm.Syntax.Expression.LetDestructuring (Node _ pattern) _ ->
                                        pattern |> Pattern.LocalExtra.usedModules

                                    Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                                        Set.union
                                            (case letValueOrFunctionDeclaration.signature of
                                                Nothing ->
                                                    Set.empty

                                                Just (Node _ signature) ->
                                                    signature.typeAnnotation
                                                        |> Type.LocalExtra.nodeUsedModules
                                            )
                                            (letValueOrFunctionDeclaration.declaration
                                                |> Elm.Syntax.Node.value
                                                |> .arguments
                                                |> List.LocalExtra.setUnionMap (\(Node _ pattern) -> pattern |> Pattern.LocalExtra.usedModules)
                                            )
                            )

                _ ->
                    Set.empty
            )


{-| Get all immediate child expressions of an expression
-}
subs : Expression -> List (Node Expression)
subs expression =
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

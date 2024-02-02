module Tests exposing (tests)

import Elm.CodeGen
import Review.Test
import Test exposing (Test)
import Upgrade


tests : Test
tests =
    Test.describe "elm-review-upgrade"
        [ Test.test "does not report new name"
            (\() ->
                """module A exposing (..)
import List.Extra exposing (findMap)

a = findMap
"""
                    |> Review.Test.run
                        (Upgrade.rule
                            [ Upgrade.reference { old = ( "MyUtil", "filterMap" ), new = ( "List.Extra", "filterMap" ) }
                            ]
                        )
                    |> Review.Test.expectNoErrors
            )
        , Test.test "updates to the new name"
            (\() ->
                """module A exposing (..)
import MyUtil as Util

a = Util.findMap
"""
                    |> Review.Test.run
                        (Upgrade.rule
                            [ Upgrade.reference { old = ( "MyUtil", "findMap" ), new = ( "List.Extra", "findMap" ) }
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "MyUtil.findMap can be upgraded to List.Extra.findMap"
                            , details =
                                [ "I suggest applying the automatic fix, then cleaning it up in a way you like."
                                ]
                            , under = "Util.findMap"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import List.Extra
import MyUtil as Util

a = (
    List.Extra.findMap)
"""
                        ]
            )
        , Test.test "uses lambda because arguments are missing, adds _ because argument name is already used"
            (\() ->
                """module A exposing (..)
import Expect as Is

a actualBool =
    Is.true
"""
                    |> Review.Test.run
                        (Upgrade.rule
                            [ Upgrade.application
                                { oldName = ( "Expect", "true" )
                                , oldArgumentNames = [ "onFalseDescription", "actualBool" ]
                                , oldArgumentsToNew =
                                    \oldArguments ->
                                        case oldArguments of
                                            [ onFalse, actual ] ->
                                                Upgrade.call ( "Expect", "equal" )
                                                    [ Elm.CodeGen.fqVal [ "Basics" ] "True", actual ]
                                                    |> Upgrade.pipeInto ( "Expect", "onFail" ) [ onFalse ]
                                                    |> Just

                                            _ ->
                                                Nothing
                                }
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expect.true can be upgraded to Expect.equal, then Expect.onFail"
                            , details =
                                [ "I suggest applying the automatic fix, then cleaning it up in a way you like."
                                ]
                            , under = "Is.true"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Expect as Is

a actualBool =
    (\\onFalseDescription actualBool_ ->
        Is.equal
            True
            actualBool_ |>
        Is.onFail
            onFalseDescription)
"""
                        ]
            )
        , Test.test "upgrade application to pipeline"
            (\() ->
                """module A exposing (..)
import Expect

a =
    list
        |> List.isEmpty
        |> Expect.true "list is filled"
"""
                    |> Review.Test.run
                        (Upgrade.rule
                            [ Upgrade.application
                                { oldName = ( "Expect", "true" )
                                , oldArgumentNames = [ "onFalseDescription", "actualBool" ]
                                , oldArgumentsToNew =
                                    \oldArguments ->
                                        case oldArguments of
                                            [ onFalse, actual ] ->
                                                Upgrade.call ( "Expect", "equal" )
                                                    [ Elm.CodeGen.fqVal [ "Basics" ] "True", actual ]
                                                    |> Upgrade.pipeInto ( "Expect", "onFail" ) [ onFalse ]
                                                    |> Just

                                            _ ->
                                                Nothing
                                }
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expect.true can be upgraded to Expect.equal, then Expect.onFail"
                            , details =
                                [ "I suggest applying the automatic fix, then cleaning it up in a way you like."
                                ]
                            , under = "Expect.true"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Expect

a =
    (
    Expect.equal
        True
        (
            list
                |> List.isEmpty) |>
    Expect.onFail
                               "list is filled")
"""
                        ]
            )
        ]

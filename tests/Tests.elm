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
        , Test.test "upgrades to the new name"
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

a = List.Extra.findMap
"""
                        ]
            )
        , Test.test "upgrades deprecated function references without deleting their arguments"
            (\() ->
                """module A exposing (..)
import List.Extra

result =
    [ 1, 2, 3 ]
        |> List.Extra.filterNot isEven
"""
                    |> Review.Test.run
                        (Upgrade.rule
                            [ Upgrade.reference { old = ( "List.Extra", "filterNot" ), new = ( "List.Extra", "removeWhen" ) }
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.Extra.filterNot can be upgraded to List.Extra.removeWhen"
                            , details =
                                [ "I suggest applying the automatic fix, then cleaning it up in a way you like."
                                ]
                            , under = "List.Extra.filterNot"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import List.Extra

result =
    [ 1, 2, 3 ]
        |> List.Extra.removeWhen isEven
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
        , Test.test "uses lambda because arguments are missing, adds _ to argument because introduced argument has the same name as explicit expose from import"
            (\() ->
                [ """module A exposing (..)
import Expect as Is
import TestDescriptionFor exposing (actualBool)

a =
    Is.true actualBool
"""
                , """module TestDescriptionFor exposing (actualBool)

actualBool =
    True
"""
                ]
                    |> Review.Test.runOnModules
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
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Expect.true can be upgraded to Expect.equal, then Expect.onFail"
                                , details =
                                    [ "I suggest applying the automatic fix, then cleaning it up in a way you like."
                                    ]
                                , under = "Is.true"
                                }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
import Expect as Is
import TestDescriptionFor exposing (actualBool)

a =
    (\\actualBool_ ->
        Is.equal
            True
            actualBool_ |>
        Is.onFail
                        actualBool)
"""
                            ]
                          )
                        ]
            )
        , Test.test "uses lambda because arguments are missing, adds _ to argument because introduced argument has the same name as expose from import exposing (..)"
            (\() ->
                [ """module A exposing (..)
import Expect as Is
import TestDescriptionFor exposing (..)

a =
    Is.true actualBool
"""
                , """module TestDescriptionFor exposing (actualBool)

actualBool =
    True
"""
                ]
                    |> Review.Test.runOnModules
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
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Expect.true can be upgraded to Expect.equal, then Expect.onFail"
                                , details =
                                    [ "I suggest applying the automatic fix, then cleaning it up in a way you like."
                                    ]
                                , under = "Is.true"
                                }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
import Expect as Is
import TestDescriptionFor exposing (..)

a =
    (\\actualBool_ ->
        Is.equal
            True
            actualBool_ |>
        Is.onFail
                        actualBool)
"""
                            ]
                          )
                        ]
            )
        , Test.test "uses lambda because arguments are missing, adds _ to argument because introduced argument has the same name as expose from import exposing (..) and that module also exposing (..)"
            (\() ->
                [ """module A exposing (..)
import Expect as Is
import TestDescriptionFor exposing (..)

a =
    Is.true actualBool
"""
                , """module TestDescriptionFor exposing (..)

actualBool =
    True
"""
                ]
                    |> Review.Test.runOnModules
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
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Expect.true can be upgraded to Expect.equal, then Expect.onFail"
                                , details =
                                    [ "I suggest applying the automatic fix, then cleaning it up in a way you like."
                                    ]
                                , under = "Is.true"
                                }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
import Expect as Is
import TestDescriptionFor exposing (..)

a =
    (\\actualBool_ ->
        Is.equal
            True
            actualBool_ |>
        Is.onFail
                        actualBool)
"""
                            ]
                          )
                        ]
            )
        , Test.test "upgrades to the new type name of a module-scope value declaration"
            (\() ->
                """module A exposing (..)
import Web

a : Web.ProgramConfig
a = a
"""
                    |> Review.Test.run
                        (Upgrade.rule
                            [ Upgrade.typeReference { old = ( "Web", "ProgramConfig" ), new = ( "Web.Program", "Config" ) }
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Web.ProgramConfig can be upgraded to Web.Program.Config"
                            , details =
                                [ "I suggest applying the automatic fix, then cleaning it up in a way you like."
                                ]
                            , under = "Web.ProgramConfig"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Web.Program
import Web

a : Web.Program.Config
a = a
"""
                        ]
            )
        , Test.test "upgrades to the new type name of a let value declaration"
            (\() ->
                """module A exposing (..)
import Web

a : Never
a =
    let
        b : Web.ProgramConfig
        b = b
    in
    a
"""
                    |> Review.Test.run
                        (Upgrade.rule
                            [ Upgrade.typeReference { old = ( "Web", "ProgramConfig" ), new = ( "Web.Program", "Config" ) }
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Web.ProgramConfig can be upgraded to Web.Program.Config"
                            , details =
                                [ "I suggest applying the automatic fix, then cleaning it up in a way you like."
                                ]
                            , under = "Web.ProgramConfig"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Web.Program
import Web

a : Never
a =
    let
        b : Web.Program.Config
        b = b
    in
    a
"""
                        ]
            )
        , Test.test "upgrades to the new type name in an aliased type"
            (\() ->
                """module A exposing (..)
import Web

type alias App =
    { initial : Int, program : Web.ProgramConfig }

a = a
"""
                    |> Review.Test.run
                        (Upgrade.rule
                            [ Upgrade.typeReference { old = ( "Web", "ProgramConfig" ), new = ( "Web.Program", "Config" ) }
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Web.ProgramConfig can be upgraded to Web.Program.Config"
                            , details =
                                [ "I suggest applying the automatic fix, then cleaning it up in a way you like."
                                ]
                            , under = "Web.ProgramConfig"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Web.Program
import Web

type alias App =
    { initial : Int, program : Web.Program.Config }

a = a
"""
                        ]
            )
        , Test.test "upgrades to the new type name of a choice type argument"
            (\() ->
                """module A exposing (..)
import Web

type App =
    App { initial : Int, program : Web.ProgramConfig }

a = a
"""
                    |> Review.Test.run
                        (Upgrade.rule
                            [ Upgrade.typeReference { old = ( "Web", "ProgramConfig" ), new = ( "Web.Program", "Config" ) }
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Web.ProgramConfig can be upgraded to Web.Program.Config"
                            , details =
                                [ "I suggest applying the automatic fix, then cleaning it up in a way you like."
                                ]
                            , under = "Web.ProgramConfig"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Web.Program
import Web

type App =
    App { initial : Int, program : Web.Program.Config }

a = a
"""
                        ]
            )
        , Test.test "upgrades to the new type of a module-scope value declaration"
            (\() ->
                """module A exposing (..)
import Map

a : Bool -> Map.Mapping from to
a = a
"""
                    |> Review.Test.run
                        (Upgrade.rule
                            [ Upgrade.type_
                                { oldName = ( "Map", "Mapping" )
                                , oldArgumentsToNew =
                                    \oldArguments ->
                                        case oldArguments of
                                            [ from, to ] ->
                                                Elm.CodeGen.funAnn from to |> Just

                                            _ ->
                                                Nothing
                                }
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Map.Mapping can be upgraded to a function type"
                            , details =
                                [ "I suggest applying the automatic fix, then cleaning it up in a way you like."
                                ]
                            , under = "Map.Mapping"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Map

a : Bool -> (from -> to)
a = a
"""
                        ]
            )
        ]

module Tests exposing (tests)

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
                            [ Upgrade.name { old = ( "MyUtil", "filterMap" ), new = ( "List.Extra", "filterMap" ) }
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
                            [ Upgrade.name { old = ( "MyUtil", "findMap" ), new = ( "List.Extra", "findMap" ) }
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
        ]

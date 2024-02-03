module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Elm.CodeGen
import Review.Rule exposing (Rule)
import Upgrade exposing (Upgrade)


config : List Rule
config =
    [ Upgrade.rule
        [ elmcraftCoreExtra1To2
        , test1To2
        ]
    ]


test1To2 : Upgrade
test1To2 =
    [ Upgrade.reference { old = ( "Fuzz", "tuple" ), new = ( "Fuzz", "pair" ) }
    , Upgrade.reference { old = ( "Fuzz", "tuple3" ), new = ( "Fuzz", "triple" ) }
    , Upgrade.application
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
        |> Upgrade.batch

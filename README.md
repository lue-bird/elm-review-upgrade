# elm-review-upgrade

[🔧 `Upgrade`](https://package.elm-lang.org/packages/lue-bird/elm-review-upgrade/1.0.0/Upgrade/ "Provides automatic fixes") reports functions and types that can be upgraded to give users an easy time migrating to a new version of a package or an internal elm API.

What this rule is not for: _simplifying_ your code to use new functions,
like replacing `YourString.join ""` by your new `YourString.concat`.

```elm
import Review.Rule
import Upgrade
import Elm.CodeGen -- the-sett/elm-syntax-dsl

config : List Review.Rule.Rule
config =
    [ Upgrade.rule
        [ Upgrade.reference { old = ( "Fuzz", "tuple" ), new = ( "Fuzz", "pair" ) }
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
    ]
```
([full examples of test 1→2 and elmcraft/core-extra 1→2](https://github.com/lue-bird/elm-review-upgrade/blob/main/preview/src/ReviewConfig.elm))

Writing custom elm-review rules for every version upgrade yourself will get tricky.
A few examples:
- Is the function curried? If it is, do transformations like reversing the argument order still work?
- Does the new module name have an import alias or is exposed? If it is exposed, is it shadowed? Does it need new imports?
- Is it used as a call, in a left or right pipeline or left/right composition?
- When changing the argument order, should we keep the formatting of the arguments?
- How do you ensure that the fix indentation doesn't interfere with previous `let` declarations, `if` branches or cases?

This rule has your back in cases like these :)

## not currently supported
- upgrading variants in any way is not supported because that's usually more involved, transforming patterns and stuff
- anything that involves more context than just the old function/type and its arguments to upgrade (e.g. when a function returns a new field and we'd need to adjust type annotations)
- 👀 something else you'd like to see?

## TODO
- upgrading types (see end-in-underscore rule: let funs, fun decls, type alias decls, type decls)

# elm-review-upgrade

[🔧 `Upgrade`](https://package.elm-lang.org/packages/lue-bird/elm-review-upgrade/1.0.0/Upgrade/ "Provides automatic fixes") reports functions that can be upgraded to give users an easy time migrating to a new version of a package or an internal elm API.

What this rule is not for: _simplifying_ your code to use new functions.
E.g. you added a `YourString.concat` for when you previously used `YourString.join ""` for that.
(Maybe we're lucky and a feature like this will come to [`elm-review.simplify`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review-simplify/latest/) some day in the distant future.)

```elm
module ReviewConfig exposing (config)

import Review.Rule
import Upgrade
import Elm.Syntax.Expression -- stil4m/elm-syntax

config : List Review.Rule.Rule
config =
    [ Upgrade.rule
        [ Upgrade.name { old = ( "Fuzz", "tuple" ), new = ( "Fuzz", "pair" ) }
        , Upgrade.application
            { oldName = ( "Expect", "true" )
            , oldArgumentNames = [ "onFalseDescription" ]
            , oldArgumentsToNew =
                \oldArguments ->
                    case oldArguments of
                        [ descriptionArgument ] ->
                            Upgrade.call ( "Expect", "equals" )
                                [ Elm.Syntax.Expression.FunctionOrValue [ "Basics" ] "True" ]
                                |> Upgrade.pipeInto ( "Expect", "onFail" )
                                    [ descriptionArgument ]
                                |> Just
                        
                        _ ->
                            Nothing
            }
        ]
    ]
```

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
- anything that involves more context than just the old function and its arguments to upgrade (e.g. when a function returns a new field and we'd need to adjust type annotations)
- 👀 something else you'd like to see?

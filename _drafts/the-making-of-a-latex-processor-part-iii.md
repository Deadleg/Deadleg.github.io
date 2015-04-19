---
layout: post
title: The making of a LaTeX pre-processor with Haskell - Part III
---

Here I attempt to tackle creating a preprocessor that converts to LaTeX by using the expressiveness of Haskell and an over-abundance of functions.

<!--end excerpt-->

### Definitions
In the previous part I suggested that math be written in Haskell as something like 

    {% highlight haskell linenos %}

    gamma   = Var
    lambda  = Var
    epsilon = Var
    x       = Var
    theta   = Func gamma

    leftEquation = Derivative theta gamma
    rightEquation = (lambda `pow` (x + epsilon)) `multiply` (Derivative (Derivative theta gamma) gamma)

    equationOne :: (Expression, Expression)
    equationOne = (leftEquation, rightEquation)

    {% endhighlight %}

Where the printed values of `gamma`, `lambda`, etc. are found by reflection and some database where `gamma` maps to `\gamma`. The first problem I came accross is you cannot have `Gamma` as a function, so you'll to prepend something like `c` get a capitalised `\Gamma`. Secondly, the `Func` constructor can hold what values it needs to, but there's no way for the user to define how it behaves. After a day of learning, I came up with this not so handy system:

math.hs:

    {% highlight haskell linenos %}

    import MathProcessor
    import Data.Data
    import MathDefaults

    gamma        = Expression { getKey="gamma"    , exprType=Var }
    lambda       = Expression { getKey="lambda"   , exprType=Var }
    epsilon      = Expression { getKey="epsilon"  , exprType=Var }
    x            = Expression { getKey="x"        , exprType=Var }
    theta        = Expression { getKey="plus"     , exprType=BinaryFunc gamma lambda constructInfix }
    leftEquation = Expression { getKey="function1", exprType=MultiFunc [theta,gamma,x,epsilon] constructMulti }

    main = do
            latex <- displayEquation leftEquation "vars.txt"
            putStrLn $ latex

    {% endhighlight %}

vars.txt:

    gamma,\gamma
    lambda,\lambda
    epsilon,\epsilon
    x,x
    plus,+
    function1,f

For the user, `math.hs` is the important file which where all the markup magic happens. The functions `constructInfix` and `constructMulti` are imported from the `MathDefaults` module to provide default behaviour when generating the LaTeX strings.

The types `Expression` and `ExpressionType` are the meat of this thing, which have definitions:

    {% highlight haskell linenos %}

    data ExpressionType = Var 
                        | UnaryFunc  Expression (Map.HashMap String String -> Expression -> (String -> String))
                        | BinaryFunc Expression Expression (Map.HashMap String String -> Expression -> Expression -> (String -> String))
                        | MultiFunc  [Expression] (Map.HashMap String String -> [Expression] -> (String -> String))

    data Expression = Expression { getKey    :: String
                                 , exprType  :: ExpressionType } 

    class LtxExpr a where
        getValue  :: Map.HashMap String String -> a -> String
        construct :: Map.HashMap String String -> a -> String

    instance LtxExpr Expression where
        getValue map expr  = case Map.lookup (getKey expr) map of
                                  Just x  -> x
                                  Nothing -> "Nothing"
        construct map expr = case exprType expr of
                                  Var                    -> getValue map expr
                                  UnaryFunc exp f        -> f map exp $ getValue map expr
                                  BinaryFunc exp1 exp2 f -> f map exp1 exp2 $ getValue map expr
                                  MultiFunc xs f         -> f map xs $ getValue map expr

    {% endhighlight %}

`ExpressionType` holds what kind of function we are dealing with. `Var` is a simple variable or constant, and `*Func` are functions which hold one or more `Expression` types. And additional function is needed for `*Func` which maps each variable, along with a hashmap which contains the LaTeX values, to another function of form `String -> String`. This final function maps whatever value the base variable is to the final string. For example, the output of such a function could be `\x -> expr1 ++ x ++ expr2`, where `expr1` and `expr2` are strings. This can be used for any infix operator, such as `+`.

The `Expression` type holds a key used by the hashmap, and an `ExpressionType`. The `LtxExpr` typeclass then defines two fucntions: `getValue`, which get the LaTeX string from the key, and `construct`, which creates the final LaTeXyfied string.

Running `Math.hs` displays:

    f(\gamma + \lambda,\gamma,x,\epsilon)

So it works! Yay.

### Is this actually useful?

When typesetting with LaTeX, each character is laid out in sequence and can be read the expected output from left to right. In my system, the characters held in a tree, so to see what the output is you would have to do a depth-first-search. This is not ideal if you're chaining together equalities in some proof, as it would be *a lot* of effort to figure out what you are writing on each line. 

Where this *could* be useful is when you spend more time defining expressions or reusing frequently used expressions in your given field, or simply adhering to whatever style guide says for formatting something like the derivative of a function.

If you're only sticking to LaTeX, an easy alternative to writing sequence of equalities is to just use `Var` and just write down the LaTeX code needed in the hashtable. For instance, in `vars.txt` have
    
    proof1,\frac{x}{2}\int^{\textbox{everything}}_0 f(x) b a

then in `math.hs`, define

    proof1 = Expression { getKey = "proof1", id }

Printing `proof` will then yield the desired result. You could even go crazy and write it into the `.tex` file itself if you wanted.

For now, I will say no, this is not useful.

### Conclusion

This series of blog posts was a relatively short journey of learning Haskell through looking into ways to improve the syntax of writing LaTeX. The end result was a partial implementation of Markdown using parsec, and a programmatic way of arranging math equations with Haskell ready to be converted to LaTeX (or MathML or probably any other markup).

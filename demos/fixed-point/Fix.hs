id' 0 = 0
id' n = id' (n - 1) + 1

f n x = (x + n / x) / 2

{-
    `fix (+1)` loops forever, because `(+1)` doesn't have fixed points.
        `let x = 1 + x in x` recursively evaluates x
    `fix id` also loops forever:
        `let x = x in x` recursively evaluates x

    Use `undefined` to represent the termination of computation.
        1 : undefined /= undefined
        const "abc" undefined /= 1
            -> because they are lazy, they don't evaluate the second argument.
        id undefined == undefined
        (+1) undefined == undefined

    With `+1`, it's like a tree with 2 levels: level 1 is `undefined`. Level 2
    contains all the other numbers. We can compare `undefined` with anything,
    but not numbers between themselves. This comparison means "it is less
    defined than". So:
        - undefined < 1, 2, 3, ...
        - undefined < undefined
        - but NOT 1 < 2 

    We get 3 levels for Maybes:
        - undefined < Nothing, Just undefined
        - Just undefined < Just 1, Just 2, ...
        - NOT Just 1 < Just 2 or Nothing < Just undefined

    For `(:1)` we get infinitely many levels:
        - undefined < [], undefined : undefined
        - undefined : undefined < 1 : undefined, undefined : [],
            undefined : undefined : undefined
        - ...

    So we order values with respect to their degree of "defined-ness".

    `fix (1:)` returns an infinite list of 1s.

    f <= g <=> f is less defined than g. Assume both are unary functions.
    F -> f <= F g

    let f0 = \_ . undefined = the most undefined function
    let f1 = F f0
    let f2 = F f1
    ...
    f0 <= f1 <= f2 <= ...

    lim(n -> inf) fn = uF = the least defined fixed point
-}
fix f = let x = f x in x
-- fix f = f (fix f)

fact n = if n == 0 then 1 else n * fact (n - 1)
-- This is not textually recursive:
bigFact f n  = if n == 0 then 1 else n * f (n - 1)

f0 = const undefined
f1 = bigFact f0
f2 = bigFact f1

{-
    f1 = \n -> if n == 0 then 1 else n * f0 (n - 1)
        - f1 is only defined for n == 0
        - similarly, f2 is only defined for n `elem` [0, 1]

    So we can use the fixed point iteration to compute the least defined fixed
    point of a higher-order function which does not employ textual recursion.

    A more elegant approach is to use a FIXED-POINT COMBINATOR:
        (Fix F) -> uF (it has been proven that a fixed-point comb always exists)
        fix f = let x = f x in x

    But this is a textually recursive definition, so we're back to square 1.
    We can define a fixed-point comb that does not use textual recursion:
        Fix = \F . (\x . (F (x x)) \x . (F (x x)))
            This is difficult to implement in Haskell, because of the type
            system. We need `x :: a -> b` but `a -> b = a` and we keep expanding
            `((a -> b) -> b) -> b` etc., i.e. an infinite type.
-}

fact' = fix bigFact
{-
    fact' 2 =  fix bigFact 2 = bigFact (fix bigFact) 2
    Evaluation is lazy so we first substitute textually:
    if 2 == 0 then 1 else 2 * (fix bigFact) (2 - 1)
        = 2 * (fix bigFact) (2 - 1) = 2 * (bigFact (fix bigFact) (2 - 1))
        = 2 * (bigFact (fix bigFact) 1)
        = 2 * (if 1 == 0 then 1 else 1 * (fix bigFact) (1 - 1))
        = 2 * (1 * (fix BigFact) 0)
        = 2 * (1 * bigFact (fix bigFact) 0)
        = 2 * (1 * 1)
    This is recursion without textual recursion (without the function calling
    itself).
-}

-- Open Recursion
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

bigFibo _ 0 = 0
bigFibo _ 1 = 1
bigFibo f n = f (n - 1) + f (n - 2)

fibo' = fix bigFibo

{-
    We want to achieve memoisation without altering the code in `fibo` or
    bigFibo`. It's like "inheritance", bot for functions. TODO: We will need
    monads to achieve this.

    `memFibo` is a wrapper over `fibo` that memoises f 29.
    This is not real memoisation. It's just hard-coding.
-}
memFibo f 30 = 832040 
memFibo f 29 = 514229
memFibo f n = f n

smartFibo = fix (memFibo . bigFibo)

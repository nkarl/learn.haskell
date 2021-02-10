
# The Idea Behind Haskell Functions & The Currying of Functions

Haskell, again, is fundamentally a mathematics-compliant language. What does that mean?

Well, it means that Haskell is written specifically to help mathematicians express their language in the form
of predicates and theorems.

Since mathematics is just so vast, Haskell is designed to be 'lazy', i.e. it only evaluates when absolutely
needed. This ushers in the idea of `currying`.

Haskell's functions are designed to take only one argument at a time. Therefore, when there are multiple
arguments passed in, the functions `curry`, i.e. taking one argument at a time in order.

For example, let's say we have a function `add x y`. This functions is in fact `(add x) y`, where `add x` is
its own function that takes `y` as its argument.

    add :: Int -> (Int -> Int)
    add x y = x + y

The expression `F = (add 11) 2` is equivalent to `f = add 11` first and *then* `g = f 2` second, where 
`F = f . g` (or g after f).

Deeper under the surface, the type signature `add :: Int -> (Int -> Int)` means:
    - takes an `Int` and turns it into a function of the type `Int -> Int`, that is 
    - a function that takes an `Int` and returns an `Int`.

Thus, the expression: 
    add :: Int -> (Int -> Int)
    add x y = x + y

is equivalent to:
    add :: Int -> (Int -> Int)
    add x y = (+) x y

and therefore means something like this:
- take `(+)`, which is a function, and binds it to `add`; let's call it f
  (this function f has type `Int -> Int`, which means that it takes an `Int` and returns an `Int`)
- pass x to the function f and turn it into an intermediary function g = `add x`
- take y and pass it to function g = `add x`, which is then pushed down all the way to the original f= `(+)` where
  result is calculated and returned

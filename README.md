FSharp.Uniplate
===============

*Fsharp.Uniplate* is a port of the Haskell [Uniplate] library by Neil Mitchell. Uniplate enabels transformations over recursive algebraic data types to be expressed in a concise way.

The library relies on reflection to automatically derive Uniplate instances for arbitrary algebraic data types.

Basic Usage
=================
Consider the following type for representing boolean expressions:

```fsharp
type Exp =
    | True
    | False
    | Var of string
    | Not of Exp
    | And of Exp * Exp
    | Or of Exp * Exp
```

A uniplate *instance* for the `Exp` data type can be derived using the `derive` function:

```fsharp
open Fsharp.Data.Generics.Uniplate
let UP = derive<Exp>
```
Using this instance a function for collecting all the names of variables of any expression value is straight forward using `UP.Universe` for collecting all sub expressions:

```fsharp

let variableNames exp =
    [ 
        for exp in UP.Universe exp do
            match exp with
            | Var x -> yield x
            | _     -> ()
    ]
```

We can also define a function that maps over all variable names of an expression, using the `Tranform` function:

```fsharp

let toUpperVarNames = UP.Transform <| function
    | Var name  -> Var <| name.ToUpper()
    | e         -> e

```
Note that we only need to pattern match on the case we are interested in, the `Transform` function will apply the function on all sub-expressions, rebuilding the expression tree from bottom up.


There is also the more powerful alternative `Rewrite` which applies the tranformations recursively until a fixed point is reached, i.e there are no sub-expressions matching any rewrite fules. Here's an example of using `Rewrite` for defining a function that normalizes boolean formulas:

```fsharp
/// Normalizes a boolean expression.
let normalize = UP.Rewrite <| function
    | Not True          -> Some False
    | Not False         -> Some True
    | And(True,e)
    | And (e,True)      -> Some e
    | And(False,_)
    | And(_, False)     -> Some False
    | Or(True,_)
    | Or(_,True)        -> Some True
    | Or(False,e)
    | Or(e,False)       -> Some e
    | Not (Not x)       -> Some x
    | Not (And (e1,e2)) -> Some (!e1 <|> !e2)
    | Not (Or (e1,e2))  -> Some (!e1 <&> !e2)
    | _                 -> None 

```

And a few examples of applying the `normalize` function:

```fsharp

> normalize <| Not (Not (Not False));;
val it : Exp = True


> normalize <| Not (And (Not (Or (Var "a",False)),Not (Var "z")))
val it : Exp = Or (Var "a",Var "z")

> normalize <| Not (Or (Not True, Var "X"));;
val it : Exp = Not (Var "X")

```

Defining Uniplate instances manually
---------------------------------------
Rather than using the `derive` function for creating a unipate instance, you may also define it manually by specifying a function for how to decompose values. Here is the manually defined counterpar for the `Exp` type:

```fsharp
let UP =
    mkUniplate <| function
        | True        -> [], fun _ -> True
        | False       -> [], fun _ -> False
        | Var n       -> [], List.head
        | Not e       -> [e], List.head >> Not
        | And (e1,e2) -> [e1;e2], fun xs -> And (xs.[0], xs.[1])
        | Or (e1,e2)  -> [e1;e2], fun xs -> Or (xs.[0], xs.[1])

```
the signature of `mkUniplate` is:

```fsharp
type Decomposition<'T> = list<'T> * (list<'T> -> 'T)

val mkUniplate<'T> (uniplate: 'T -> Decomposition<'T>) : Uniplate<'T> =
```
The function argument needs to map an value to a tuple of a list of sub-expressions (children) and constructor function
for composing children to create an expression of the same type.

The `UP` above is equivalent with the derived version allthough it's faster since it doesn't rely on reflectoin for decmposing the values.









[Uniplate]:https://hackage.haskell.org/package/uniplate

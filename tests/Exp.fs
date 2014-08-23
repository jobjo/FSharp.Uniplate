namespace Fsharp.Data.Generics.Uniplate.Tests

module Exp =

    // Representing boolean expressions.
    type Exp =
        | True
        | False
        | Var of string
        | Not of Exp
        | And of Exp * Exp
        | Or of Exp * Exp
    let (<&>) e1 e2 = And (e1,e2)
    let (<|>) e1 e2 = Or (e1,e2)
    let (!) e = Not e

    /// Evaluate an expression given a list of variable and value pairs.
    let evaluate env exp =
        // Lookup the value of a variable.
        let lookup name =
            match List.tryPick (fun (n,v) -> if n = name then Some v else None) env with
            | None      -> false
            | Some v    -> v
        let rec go exp k =
            match exp with
            | True              -> k true
            | False             -> k false
            | Var n             -> k (lookup n)
            | Not e             -> go e (fun x -> k (not x))
            | And (e1,e2)       -> go e1 <| fun x -> go e2 <| fun y -> k (x && y)
            | Or(e1,e2)         ->  go e1 <| fun x -> go e2 <| fun y -> k (x || y)
        go exp id




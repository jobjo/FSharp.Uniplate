namespace Fsharp.Data.Generics.Uniplate.Examples

module Exp =
    open Utils
    open Fsharp.Data.Generics.Uniplate


    type Exp =
        | True
        | False
        | Var of string
        | Not of Exp
        | And of Exp * Exp
        | Or of Exp * Exp
        override this.ToString() =
            let rec printBin name e1 e2=
                !<[
                    !(sprintf "%s[" name)
                    indent (go e1)
                    indent (go e2)
                    !"]"
                ]
            and go = function
                | True          -> !"True"
                | False         -> !"False"
                | Var v         -> !v
                | Not e         -> !<[!"Not[" ; indent (go e); !"]"]
                | Or (e1,e2)    -> printBin "Or" e1 e2
                | And (e1,e2)   -> printBin "And" e1 e2
            run <| go this

    let (<&>) e1 e2 = And (e1,e2)
    let (<|>) e1 e2 = Or (e1,e2)
    let (!) e = Not e







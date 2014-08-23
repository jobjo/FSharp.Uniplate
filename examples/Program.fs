namespace Fsharp.Data.Generics.Uniplate.Examples

module Program =

    open Exp
    open Fsharp.Data.Generics.Uniplate

    /// Create a uniplate instance fro Exp derived from it's type (using reflection).
    let UPG = derive<Exp>

    /// This is an equivalent instance defined manually.
    let UPM =
        mkUniplate <| function
            | True                  -> [], fun _ -> True
            | False                 -> [], fun _ -> False
            | Var n                 -> [], List.head
            | Not e                 -> [e], List.head >> Not
            | And (e1,e2)           -> [e1;e2], fun xs -> And (xs.[0], xs.[1])
            | Or (e1,e2)            -> [e1;e2], fun xs -> Or (xs.[0], xs.[1])

    /// Normalizes a boolean expression.
    let normalize = UPG.Rewrite <| function
        | Not True              -> Some False
        | Not False             -> Some True
        | And(True,e)
        | And (e,True)          -> Some e
        | And(False,_)
        | And(_, False)         -> Some False
        | Or(True,_)
        | Or(_,True)            -> Some True
        | Or(False,e)
        | Or(e,False)           -> Some e
        | Not (Not x)           -> Some x
        | Not (And (e1,e2))     -> Some (!e1 <|> !e2)
        | Not (Or (e1,e2))      -> Some (!e1 <&> !e2)
        | _                     -> None 

    /// Change all variables to upper case letters.
    let toUpperVarNames = UPG.Transform <| function
        | Var name  -> Var <| name.ToUpper()
        | e         -> e

    /// Collect all variable names of an expression.
    let allVariables =
        UPG.Universe
        >> List.choose (function
            | Var x -> Some x
            | _     -> None
        )

    /// Shows some 
    [<EntryPoint>]
    let main args =
        let test e =
            printfn "========================="
            printfn "Original expression:"
            printfn "%s" (string e)
            printfn "Variables: %A" (allVariables e)
            printfn "Normalized expression:"
            printfn "%s" (normalize e |> toUpperVarNames |> string)
        [
            Var "x"
            !(!(Var "y"))
            !False
            !(!False)
            !(Var "z" <|> !(Var "k"))
            Var "a" <|> False
            !(!(Var "a" <|> False) <&> !(Var "z"))
            !(!(!False) <|> !(Var "v"))
        ]
        |> Seq.iter test
        0


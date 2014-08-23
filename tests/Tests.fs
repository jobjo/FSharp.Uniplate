namespace Fsharp.Data.Generics.Uniplate.Tests

module Tests =
    open FsCheck.Xunit
    open Exp
    open Fsharp.Data.Generics.Uniplate

    /// Collect all variable names of an expression.
    let allVariables (up: Uniplate<Exp>) exp=
       up.Universe exp
        |> List.choose (function
            | Var x -> Some x
            | _     -> None
        )
    /// Create environment (variable, name pairs)
    let createEnv up exp =
        allVariables up exp
        |> List.filter (fun x -> System.String.IsNullOrEmpty(x) |> not)
        |> List.map (fun x ->
            x, (x.Length % 2 = 0)
        )

    /// Normalizes a boolean expression.
    let normalize (up: Uniplate<Exp>) = up.Rewrite <| function
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

    /// Manually defined uniplate instance.
    let UPMan =
        mkUniplate <| function
            | True                  -> [], fun _ -> True
            | False                 -> [], fun _ -> False
            | Var n                 -> [], List.head
            | Not e                 -> [e], List.head >> Not
            | And (e1,e2)           -> [e1;e2], fun xs -> And (xs.[0], xs.[1])
            | Or (e1,e2)            -> [e1;e2], fun xs -> Or (xs.[0], xs.[1])

    let UPGen = derive<Exp>

    [<Property>]
    let ``Check that normalization of generic and manual uniplte instances evaluate to the same result`` (exp: Exp) =
        // Creates an environment
        let env = createEnv UPMan exp

        // Normalization using manual and derived uniplate instances.
        let expNMan = normalize UPMan exp
        let expNGen = normalize UPGen exp

        // Verify that results are consistent and both normalized expressions are identical
        let res = evaluate env exp
        expNMan = expNGen

    [<Property>]
    let ``Check normalization preserves semantics for manual instance of uniplate`` (exp: Exp) =
        let env = createEnv UPMan exp
        let exp' = normalize UPMan exp
        evaluate env exp = evaluate env exp'

    [<Property>]
    let ``Check normalization preserves semantics for generic instance of uniplate`` (exp: Exp) =
        let env = createEnv UPGen exp
        let exp' = normalize UPGen exp
        evaluate env exp = evaluate env exp'

    [<Property>]
    let ``Check that universes of manual and generic unplate instances are identical`` (exp: Exp) =
        UPMan.Universe exp = UPGen.Universe exp

    [<Property>]
    let ``Rewriting all variables to Barbara works for manual instance of uniplate`` (exp: Exp) =
        exp
        |> UPMan.Transform (function
            | Var _     -> Var "Barbara"
            | exp       -> exp
        )
        |> UPMan.Universe
        |> List.forall (function
            | Var x -> x = "Barbara"
            | _     -> true
        )

    [<Property>]
    let ``Rewriting all variables to Barbara works for generic instance of uniplate`` (exp: Exp) =
        exp
        |> UPGen.Transform (function
            | Var _     -> Var "Barbara"
            | exp       -> exp
        )
        |> UPGen.Universe
        |> List.forall (function
            | Var x -> x = "Barbara"
            | _     -> true
        )

    [<Property>]
    let ``Check that children of expressions are correct using manual instance of uniplate`` (exp: Exp) =
        let children =
            match exp with
            | True          -> []
            | False         -> []
            | Var _         -> []
            | Not e         -> [e]
            | And (e1,e2)
            | Or (e1, e2)   -> [e1; e2]
        UPMan.Children exp = children

    [<Property>]
    let ``Check that children of expressions are correct using generic instance of uniplate`` (exp: Exp) =
        let children =
            match exp with
            | True          -> []
            | False         -> []
            | Var _         -> []
            | Not e         -> [e]
            | And (e1,e2)
            | Or (e1, e2)   -> [e1; e2]
        
        printfn "-----------------------------"
        printfn "Expression: %A" exp
        printfn "%A" (UPGen.Children exp)
        printfn "%A" children
        
        UPGen.Children exp = children

    /// Forces evaluation of top level let-bindings.
    [<EntryPoint>]
    let main args = 0


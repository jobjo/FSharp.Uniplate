namespace Fsharp.Data.Generics

module Uniplate =

    open Microsoft.FSharp.Reflection
    open System.Reflection
    open Utils

    // A pair of a list of child expressions and
    // a constructor for building an expression from 
    // a list of sub expression.
    type Decomposition<'T> = list<'T> * (list<'T> -> 'T)

    /// Uniplate interface.
    type Uniplate<'T> =
        {
            /// List of all sub expressions.
            Universe : 'T -> list<'T>
            
            /// Applies transformation in a bottom up fashion.
            Transform : ('T -> 'T) -> 'T -> 'T
            
            /// Applies transformations until a fixed point is reached.
            Rewrite : ('T -> option<'T>) -> 'T -> 'T
            
            /// Returns all child nodes of an expression.
            Children : 'T -> list<'T>
        }

    /// Derive a union plate instance from a type using reflection.
    let private deriveUniplate<'T>  =

        // Base type of discriminated union
        let baseType = typeof<'T>

        // Raise an exception if the given type is not a discriminated union.
        if FSharpType.IsUnion (baseType) |> not then
            failwith "Cannot derive Uniplate for non discriminated union type"

        // Memoize union cases
        let withCase =
            let map =
                FSharpType.GetUnionCases baseType
                |> Array.map (fun case -> case.Tag, case.GetFields())
                |> Map.ofSeq
            fun tag -> (|>) (Map.find tag map)

        // Return a function that given a value of a derived type, returns
        // a list of tuples of child expressions and a composition function
        // by using reflection on the given value.
        fun (exp: 'T) ->
            // The runtime type of the given expression. This may be different
            // from the base type since different union cases compiles to
            // distinct classes.
            let expType = exp.GetType ()

            // Cast to base type
            let toExpType (x: obj) = x :?> 'T

            // Given a property info object, returns whether the property type
            // is same as base type.
            let isExpTypeField (field: PropertyInfo) =
                field.PropertyType = baseType

            // Retrieve the union case info for the given value.
            let info  = fst <| FSharpValue.GetUnionFields(exp, expType)

            // Generate the list of children and composition functions for
            // the given union case.
            withCase info.Tag <| fun fields ->
                // Collect all child expressions of the base type
                // and other parameters.
                let children, defParams =
                    let chs, dps =
                        (([], []), fields)
                        ||> Array.fold (fun (children, defParams) field ->
                            try
                                field.GetValue(exp, [||]) |> ignore
                            with
                            | e ->
                                printfn "Could not get value %A of field %A" exp field
                            let vl = field.GetValue(exp, [||])
                            if isExpTypeField field then
                                (toExpType vl :: children, defParams)
                            else
                                (children, vl :: defParams)
                        )
                    List.rev chs, List.rev dps

                // Defines a function for constructing the expression
                // from a list of parameters.
                let builder (xs: list<'T>) =
                    // If the union case does not contain any parameters
                    if fields.Length = 0 then
                        exp
                    else
                        // Collect parameters from either the default ones or
                        // the given list of sub expressions.
                        let (ps, _, _) =
                            (([], defParams, xs) , fields)
                            ||> Array.fold (fun (ps, ds, xs) field ->
                                // If the parameter is of the same type as 
                                // the expression.
                                if isExpTypeField field then
                                    let p = List.head xs |> box
                                    (p :: ps, ds, List.tail xs)
                                else
                                    let d = List.head ds
                                    (d :: ps, List.tail ds, xs)
                            )
                        // Reverse and transform to array.
                        let ps = 
                            ps |> List.map box |> List.rev |> Array.ofList
                        
                        // Build union expression from parameters.
                        FSharpValue.MakeUnion (info, ps)
                        |> toExpType
                children, builder

    /// Constructs a uniplate interface given a function for
    /// decomposing expressions.
    let mkUniplate<'T> (uniplate: 'T -> Decomposition<'T>) : Uniplate<'T> =

        let children = uniplate >> fst

        let rec universe x =
            x :: List.collect universe (children x)

        let transform f x =
            let rec go x =
                let children, build = uniplate x
                match children with
                | []        -> f x
                | cs        -> List.map go cs |> build |> f
            go x

        let rec rewrite f =
            let g x = maybe x (rewrite f) (f x)
            transform g >> g
        {
            Universe = universe
            Transform = transform
            Rewrite = rewrite
            Children = children
        }

    let derive<'T> = mkUniplate deriveUniplate<'T>
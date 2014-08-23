namespace Fsharp.Data.Generics

module internal Utils =
    let maybe y f x =
        match x with
        | Some x    -> f x
        | None      -> y

    let tryApply f x = maybe x id (f x)
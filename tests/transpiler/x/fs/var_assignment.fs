// Mochi 0.10.31 - generated 2025-07-19 07:54:40 UTC
open System

let print (x: obj) =
    match x with
    | :? bool as b -> printfn "%d" (if b then 1 else 0)
    | :? float as f -> printfn "%.1f" f
    | :? string as s -> printfn "%s" s
    | :? System.Collections.IEnumerable as e ->
        e |> Seq.cast<obj> |> Seq.map string |> String.concat " " |> printfn "%s"
    | _ -> printfn "%O" x

let mutable x = 1
x <- 2
print x

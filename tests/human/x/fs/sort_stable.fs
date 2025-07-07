let items = [| {| n = 1; v = "a" |}; {| n = 1; v = "b" |}; {| n = 2; v = "c" |} |]
let result = items |> Array.sortBy (fun i -> i.n) |> Array.map (fun i -> i.v)
printfn "%A" result

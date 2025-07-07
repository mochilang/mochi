let data = [ {| a = 1; b = 2 |}; {| a = 1; b = 1 |}; {| a = 0; b = 5 |} ]
let sorted = data |> List.sortBy (fun x -> (x.a, x.b))
printfn "%A" sorted

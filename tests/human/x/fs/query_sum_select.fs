let nums = [1;2;3]
let result = nums |> List.filter (fun n -> n > 1) |> List.sum
printfn "%d" result

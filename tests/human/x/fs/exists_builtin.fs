let data = [1; 2]
let flag = data |> List.exists (fun x -> x = 1)
printfn "%b" flag


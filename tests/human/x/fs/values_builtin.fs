let m = Map.ofList [("a",1); ("b",2); ("c",3)]
let values = m |> Map.toList |> List.map snd
printfn "%s" (String.concat " " (List.map string values))

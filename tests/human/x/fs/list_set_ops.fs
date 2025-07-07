open System

let union = Set.union (Set.ofList [1;2]) (Set.ofList [2;3]) |> Set.toList
printfn "%A" union

let exceptSet = Set.difference (Set.ofList [1;2;3]) (Set.ofList [2]) |> Set.toList
printfn "%A" exceptSet

let inter = Set.intersect (Set.ofList [1;2;3]) (Set.ofList [2;4]) |> Set.toList
printfn "%A" inter

let unionAllLen = ([1;2] @ [2;3]) |> List.length
printfn "%d" unionAllLen

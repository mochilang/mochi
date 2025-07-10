open System

let _union (a: 'T list) (b: 'T list) : 'T list =
    List.distinct (a @ b)

let _except (a: 'T list) (b: 'T list) : 'T list =
    List.filter (fun x -> not (List.contains x b)) a

let _intersect (a: 'T list) (b: 'T list) : 'T list =
    a |> List.filter (fun x -> List.contains x b) |> List.distinct

printfn "%A" ((_union [1; 2] [2; 3]))
printfn "%A" ((_except [1; 2; 3] [2]))
printfn "%A" ((_intersect [1; 2; 3] [2; 4]))
printfn "%A" (List.length ([1; 2] @ [2; 3]))

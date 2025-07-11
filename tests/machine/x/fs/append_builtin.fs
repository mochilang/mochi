open System

let a: int list = [1; 2]
printfn "%s" (String.concat " " (List.map string (a @ [3])))

open System

let a = [1; 2]
let result = a @ [3]
printfn "%s" (String.concat " " (List.map string result))

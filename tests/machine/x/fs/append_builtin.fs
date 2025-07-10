
let a = [1; 2]
printfn "%s" (String.concat " " (List.map string (a @ [3])))

// Generated 2025-07-21 18:37 +0700

let a: int list = [1; 2]
printfn "%s" (("[" + (String.concat ", " (List.map string (a @ [3])))) + "]")

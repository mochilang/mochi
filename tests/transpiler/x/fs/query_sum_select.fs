// Generated 2025-07-21 18:37 +0700

let nums: int list = [1; 2; 3]
let result = [ for n in nums do if n > 1 then yield Seq.sum n ]
printfn "%s" (("[" + (String.concat ", " (List.map string result))) + "]")

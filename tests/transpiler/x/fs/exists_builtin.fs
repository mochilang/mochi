// Generated 2025-07-21 18:37 +0700

let data: int list = [1; 2]
let flag: bool = not (List.isEmpty [ for x in data do if x = 1 then yield x ])
printfn "%b" flag

open System

let data: int list = [1; 2]
let flag: obj = not (List.isEmpty [ for x in data do if x = 1 then yield x ])
printfn "%A" (flag)

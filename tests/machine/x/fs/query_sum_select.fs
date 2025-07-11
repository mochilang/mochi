open System

let nums: int list = [1; 2; 3]
let result: obj list = [ for n in nums do if n > 1 then yield List.sum n ]
printfn "%A" (result)

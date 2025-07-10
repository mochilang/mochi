
let nums = [1; 2; 3]
let result = [ for n in nums do if n > 1 then yield List.sum n ]
printfn "%A" (result)

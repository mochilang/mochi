open System

let nums: int list = [1; 2; 3]
printfn "%b" (List.contains 2 nums)
printfn "%b" (List.contains 4 nums)

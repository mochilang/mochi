
let twoSum (nums) (target) =
    let n = List.length nums
    for i in 0 .. n do
        for j in i + 1 .. n do
            if nums.[i] + nums.[j] = target then
                [i; j]
    [-1; -1]
let result = twoSum [2; 7; 11; 15] 9
printfn "%A" (result.[0])
printfn "%A" (result.[1])

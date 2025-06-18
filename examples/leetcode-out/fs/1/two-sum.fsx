open System

exception Return_twoSum of int[]
let twoSum (nums: int[]) (target: int) : int[] =
    try
        let n = nums.Length
        for i = 0 to n - 1 do
            for j = (i + 1) to n - 1 do
                if ((nums.[i] + nums.[j]) = target) then
                    raise (Return_twoSum ([|i; j|]))
        raise (Return_twoSum ([|(-1); (-1)|]))
        failwith "unreachable"
    with Return_twoSum v -> v

let result = twoSum [|2; 7; 11; 15|] 9
printfn "%A" result.[0]
printfn "%A" result.[1]

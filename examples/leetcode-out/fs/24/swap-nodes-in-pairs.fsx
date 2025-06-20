open System

exception Return_swapPairs of int[]
let swapPairs (nums: int[]) : int[] =
    try
        let mutable i = 0
        let mutable result = [||]
        while (i < nums.Length) do
            if ((i + 1) < nums.Length) then
                result <- Array.append result [|nums.[(i + 1)]; nums.[i]|]
            else
                result <- Array.append result [|nums.[i]|]
            i <- (i + 2)
        raise (Return_swapPairs (result))
        failwith "unreachable"
    with Return_swapPairs v -> v


open System

exception Return_reverseKGroup of int[]
let reverseKGroup (nums: int[]) (k: int) : int[] =
    try
        let n = nums.Length
        if (k <= 1) then
            raise (Return_reverseKGroup (nums))
        let mutable result = [||]
        let mutable i = 0
        while (i < n) do
            let _end = (i + k)
            if (_end <= n) then
                let mutable j = (_end - 1)
                while (j >= i) do
                    result <- Array.append result [|nums.[j]|]
                    j <- (j - 1)
            else
                let mutable j = i
                while (j < n) do
                    result <- Array.append result [|nums.[j]|]
                    j <- (j + 1)
            i <- (i + k)
        raise (Return_reverseKGroup (result))
        failwith "unreachable"
    with Return_reverseKGroup v -> v


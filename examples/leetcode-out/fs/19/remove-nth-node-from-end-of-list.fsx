open System

exception Return_removeNthFromEnd of int[]
let removeNthFromEnd (nums: int[]) (n: int) : int[] =
    try
        let idx = (nums.Length - n)
        let mutable result = [||]
        let mutable i = 0
        while (i < nums.Length) do
            if (i <> idx) then
                result <- Array.append result [|nums.[i]|]
            i <- (i + 1)
        raise (Return_removeNthFromEnd (result))
        failwith "unreachable"
    with Return_removeNthFromEnd v -> v


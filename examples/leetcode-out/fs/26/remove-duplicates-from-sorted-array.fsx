open System

exception Return_removeDuplicates of int
let removeDuplicates (nums: int[]) : int =
    try
        if (nums.Length = 0) then
            raise (Return_removeDuplicates (0))
        let mutable count = 1
        let mutable prev = nums.[0]
        let mutable i = 1
        while (i < nums.Length) do
            let cur = nums.[i]
            if (cur <> prev) then
                count <- (count + 1)
                prev <- cur
            i <- (i + 1)
        raise (Return_removeDuplicates (count))
        failwith "unreachable"
    with Return_removeDuplicates v -> v


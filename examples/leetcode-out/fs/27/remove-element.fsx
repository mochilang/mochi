open System

exception Return_removeElement of int
let removeElement (nums: int[]) (val: int) : int =
    try
        let mutable k = 0
        let mutable i = 0
        while (i < nums.Length) do
            if (nums.[i] <> val) then
                nums.[k] <- nums.[i]
                k <- (k + 1)
            i <- (i + 1)
        raise (Return_removeElement (k))
        failwith "unreachable"
    with Return_removeElement v -> v


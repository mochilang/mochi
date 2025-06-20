open System

exception Return_mergeTwoLists of int[]
let mergeTwoLists (l1: int[]) (l2: int[]) : int[] =
    try
        let mutable i = 0
        let mutable j = 0
        let mutable result = [||]
        while ((i < l1.Length) && (j < l2.Length)) do
            if (l1.[i] <= l2.[j]) then
                result <- Array.append result [|l1.[i]|]
                i <- (i + 1)
            else
                result <- Array.append result [|l2.[j]|]
                j <- (j + 1)
        while (i < l1.Length) do
            result <- Array.append result [|l1.[i]|]
            i <- (i + 1)
        while (j < l2.Length) do
            result <- Array.append result [|l2.[j]|]
            j <- (j + 1)
        raise (Return_mergeTwoLists (result))
        failwith "unreachable"
    with Return_mergeTwoLists v -> v


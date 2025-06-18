open System

exception Return_addTwoNumbers of int[]
let addTwoNumbers (l1: int[]) (l2: int[]) : int[] =
    try
        let mutable i = 0
        let mutable j = 0
        let mutable carry = 0
        let mutable result = [||]
        while (((i < l1.Length) || (j < l2.Length)) || (carry > 0)) do
            let mutable x = 0
            if (i < l1.Length) then
                x <- l1.[i]
                i <- (i + 1)
            let mutable y = 0
            if (j < l2.Length) then
                y <- l2.[j]
                j <- (j + 1)
            let sum = ((x + y) + carry)
            let digit = (sum % 10)
            carry <- (sum / 10)
            result <- Array.append result [|digit|]
        raise (Return_addTwoNumbers (result))
        failwith "unreachable"
    with Return_addTwoNumbers v -> v


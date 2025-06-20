open System
exception BreakException of int
exception ContinueException of int

exception Return_threeSum of int[][]
let threeSum (nums: int[]) : int[][] =
    try
        let sorted = [ for x in nums do yield (x, x) ] |> List.sortBy fst |> List.map snd
        let n = sorted.Length
        let mutable res = [||]
        let mutable i = 0
        try
            while (i < n) do
                try
                    if ((i > 0) && (sorted.[i] = sorted.[(i - 1)])) then
                        i <- (i + 1)
                        raise (ContinueException 0)
                    let mutable left = (i + 1)
                    let mutable right = (n - 1)
                    while (left < right) do
                        let sum = ((sorted.[i] + sorted.[left]) + sorted.[right])
                        if (sum = 0) then
                            res <- Array.append res [|[|sorted.[i]; sorted.[left]; sorted.[right]|]|]
                            left <- (left + 1)
                            while ((left < right) && (sorted.[left] = sorted.[(left - 1)])) do
                                left <- (left + 1)
                            right <- (right - 1)
                            while ((left < right) && (sorted.[right] = sorted.[(right + 1)])) do
                                right <- (right - 1)
                        elif (sum < 0) then
                            left <- (left + 1)
                        else
                            right <- (right - 1)
                    i <- (i + 1)
                with ContinueException n when n = 0 -> ()
        with BreakException n when n = 0 -> ()
        raise (Return_threeSum (res))
        failwith "unreachable"
    with Return_threeSum v -> v


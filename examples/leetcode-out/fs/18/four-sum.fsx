open System
exception BreakException of int
exception ContinueException of int

exception Return_fourSum of int[][]
let fourSum (nums: int[]) (target: int) : int[][] =
    try
        let sorted = [ for n in nums do yield (n, n) ] |> List.sortBy fst |> List.map snd
        let n = sorted.Length
        let mutable result = [||]
        try
            for i = 0 to n - 1 do
                try
                    if ((i > 0) && (sorted.[i] = sorted.[(i - 1)])) then
                        raise (ContinueException 0)
                    try
                        for j = (i + 1) to n - 1 do
                            try
                                if ((j > (i + 1)) && (sorted.[j] = sorted.[(j - 1)])) then
                                    raise (ContinueException 1)
                                let mutable left = (j + 1)
                                let mutable right = (n - 1)
                                while (left < right) do
                                    let sum = (((sorted.[i] + sorted.[j]) + sorted.[left]) + sorted.[right])
                                    if (sum = target) then
                                        result <- Array.append result [|[|sorted.[i]; sorted.[j]; sorted.[left]; sorted.[right]|]|]
                                        left <- (left + 1)
                                        right <- (right - 1)
                                        while ((left < right) && (sorted.[left] = sorted.[(left - 1)])) do
                                            left <- (left + 1)
                                        while ((left < right) && (sorted.[right] = sorted.[(right + 1)])) do
                                            right <- (right - 1)
                                    elif (sum < target) then
                                        left <- (left + 1)
                                    else
                                        right <- (right - 1)
                            with ContinueException n when n = 1 -> ()
                    with BreakException n when n = 1 -> ()
                with ContinueException n when n = 0 -> ()
        with BreakException n when n = 0 -> ()
        raise (Return_fourSum (result))
        failwith "unreachable"
    with Return_fourSum v -> v


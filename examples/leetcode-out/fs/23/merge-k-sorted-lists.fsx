open System
exception BreakException of int
exception ContinueException of int

exception Return_mergeKLists of int[]
let mergeKLists (lists: int[][]) : int[] =
    try
        let k = lists.Length
        let mutable indices = [||]
        let mutable i = 0
        while (i < k) do
            indices <- Array.append indices [|0|]
            i <- (i + 1)
        let mutable result = [||]
        try
            while true do
                try
                    let mutable best = 0
                    let mutable bestList = (-1)
                    let mutable found = false
                    let mutable j = 0
                    while (j < k) do
                        let idx = indices.[j]
                        if (idx < lists.[j].Length) then
                            let val = lists.[j].[idx]
                            if ((not found) || (val < best)) then
                                best <- val
                                bestList <- j
                                found <- true
                        j <- (j + 1)
                    if (not found) then
                        raise (BreakException 0)
                    result <- Array.append result [|best|]
                    indices.[bestList] <- (indices.[bestList] + 1)
                with ContinueException n when n = 0 -> ()
        with BreakException n when n = 0 -> ()
        raise (Return_mergeKLists (result))
        failwith "unreachable"
    with Return_mergeKLists v -> v


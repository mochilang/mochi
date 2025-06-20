open System

exception Return_threeSumClosest of int
let threeSumClosest (nums: int[]) (target: int) : int =
    try
        let sorted = [ for n in nums do yield (n, n) ] |> List.sortBy fst |> List.map snd
        let n = sorted.Length
        let mutable best = ((sorted.[0] + sorted.[1]) + sorted.[2])
        for i = 0 to n - 1 do
            let mutable left = (i + 1)
            let mutable right = (n - 1)
            while (left < right) do
                let sum = ((sorted.[i] + sorted.[left]) + sorted.[right])
                if (sum = target) then
                    raise (Return_threeSumClosest (target))
                let mutable diff = 0
                if (sum > target) then
                    diff <- (sum - target)
                else
                    diff <- (target - sum)
                let mutable bestDiff = 0
                if (best > target) then
                    bestDiff <- (best - target)
                else
                    bestDiff <- (target - best)
                if (diff < bestDiff) then
                    best <- sum
                if (sum < target) then
                    left <- (left + 1)
                else
                    right <- (right - 1)
        raise (Return_threeSumClosest (best))
        failwith "unreachable"
    with Return_threeSumClosest v -> v


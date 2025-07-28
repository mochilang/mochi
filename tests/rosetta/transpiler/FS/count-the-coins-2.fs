// Generated 2025-07-28 11:14 +0700

exception Return

let rec countChange (amount: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable amount = amount
    try
        let mutable ways: int array = [||]
        let mutable i: int = 0
        while i <= amount do
            ways <- Array.append ways [|0|]
            i <- i + 1
        ways.[0] <- 1
        for coin in [|100; 50; 25; 10; 5; 1|] do
            let mutable j = coin
            while (int j) <= amount do
                ways.[j] <- (ways.[j]) + (ways.[j - coin])
                j <- (int j) + 1
        __ret <- ways.[amount]
        raise Return
        __ret
    with
        | Return -> __ret
let amount: int = 1000
printfn "%s" ((("amount, ways to make change: " + (string amount)) + " ") + (string (countChange amount)))

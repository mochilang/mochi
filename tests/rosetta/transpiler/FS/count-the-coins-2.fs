// Generated 2025-07-30 21:41 +0700

exception Return

let mutable _nowSeed:int64 = 0L
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- int64 v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525L + 1013904223L) % 2147483647L
        int _nowSeed
    else
        int (System.DateTime.UtcNow.Ticks % 2147483647L)

_initNow()
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
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
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

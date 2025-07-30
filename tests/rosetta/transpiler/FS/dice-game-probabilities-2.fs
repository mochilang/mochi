// Generated 2025-07-31 00:10 +0700

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
open System

let rec roll (nDice: int) (nSides: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable nDice = nDice
    let mutable nSides = nSides
    try
        let mutable sum: int = 0
        let mutable i: int = 0
        while i < nDice do
            sum <- int ((int (sum + (int ((((_now()) % nSides + nSides) % nSides))))) + 1)
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec beats (n1: int) (s1: int) (n2: int) (s2: int) (trials: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n1 = n1
    let mutable s1 = s1
    let mutable n2 = n2
    let mutable s2 = s2
    let mutable trials = trials
    try
        let mutable wins: int = 0
        let mutable i: int = 0
        while i < trials do
            if (roll n1 s1) > (roll n2 s2) then
                wins <- wins + 1
            i <- i + 1
        __ret <- (float wins) / (float trials)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (string (beats 9 4 6 6 1000))
printfn "%s" (string (beats 5 10 7 6 1000))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

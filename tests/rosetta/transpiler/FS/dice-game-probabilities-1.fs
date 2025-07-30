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
let rec powInt (``base``: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable r: int = 1
        let mutable b: int = ``base``
        let mutable e: int = exp
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                r <- r * b
            b <- b * b
            e <- e / (int 2)
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
let rec minInt (x: int) (y: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    let mutable y = y
    try
        __ret <- if x < y then x else y
        raise Return
        __ret
    with
        | Return -> __ret
let rec throwDie (nSides: int) (nDice: int) (s: int) (counts: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable nSides = nSides
    let mutable nDice = nDice
    let mutable s = s
    let mutable counts = counts
    try
        if nDice = 0 then
            counts.[s] <- (counts.[s]) + 1
            __ret <- ()
            raise Return
        let mutable i: int = 1
        while i <= nSides do
            throwDie nSides (nDice - 1) (s + i) counts
            i <- i + 1
        __ret
    with
        | Return -> __ret
let rec beatingProbability (nSides1: int) (nDice1: int) (nSides2: int) (nDice2: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable nSides1 = nSides1
    let mutable nDice1 = nDice1
    let mutable nSides2 = nSides2
    let mutable nDice2 = nDice2
    try
        let len1: int = (nSides1 + 1) * nDice1
        let mutable c1: int array = [||]
        let mutable i: int = 0
        while i < len1 do
            c1 <- Array.append c1 [|0|]
            i <- i + 1
        throwDie nSides1 nDice1 0 c1
        let len2: int = (nSides2 + 1) * nDice2
        let mutable c2: int array = [||]
        let mutable j: int = 0
        while j < len2 do
            c2 <- Array.append c2 [|0|]
            j <- j + 1
        throwDie nSides2 nDice2 0 c2
        let p12: float = (float (powInt nSides1 nDice1)) * (float (powInt nSides2 nDice2))
        let mutable tot: float = 0.0
        i <- 0
        while i < len1 do
            j <- 0
            let m: int = minInt i len2
            while j < m do
                tot <- tot + (((float (c1.[i])) * (float (c2.[j]))) / p12)
                j <- j + 1
            i <- i + 1
        __ret <- tot
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (string (beatingProbability 4 9 6 6))
printfn "%s" (string (beatingProbability 10 5 7 6))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

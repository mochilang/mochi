// Generated 2025-07-28 10:03 +0700

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
let n: int = 64
let rec pow2 (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable k = k
    try
        let mutable v: int = 1
        let mutable i: int = 0
        while i < k do
            v <- v * 2
            i <- i + 1
        __ret <- v
        raise Return
        __ret
    with
        | Return -> __ret
let rec ruleBit (ruleNum: int) (idx: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ruleNum = ruleNum
    let mutable idx = idx
    try
        let mutable r: int = ruleNum
        let mutable i: int = 0
        while i < idx do
            r <- r / 2
            i <- i + 1
        __ret <- ((r % 2 + 2) % 2)
        raise Return
        __ret
    with
        | Return -> __ret
let rec evolve (state: int array) (ruleNum: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable state = state
    let mutable ruleNum = ruleNum
    try
        let mutable out: int array = [||]
        let mutable p: int = 0
        while p < 10 do
            let mutable b: int = 0
            let mutable q: int = 7
            while q >= 0 do
                let st: int array = state
                b <- b + (int ((st.[0]) * (int (pow2 q))))
                let mutable next: int array = [||]
                let mutable i: int = 0
                while i < n do
                    let mutable lidx: int = i - 1
                    if lidx < 0 then
                        lidx <- n - 1
                    let left: int = st.[lidx]
                    let center: int = st.[i]
                    let ridx: int = i + 1
                    if ridx >= n then
                        ridx <- 0
                    let right: int = st.[ridx]
                    let index: int = ((left * 4) + (center * 2)) + right
                    next <- Array.append next [|unbox<int> (ruleBit ruleNum index)|]
                    i <- i + 1
                state <- next
                q <- q - 1
            out <- Array.append out [|b|]
            p <- p + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let mutable init: int array = [||]
let mutable i: int = 0
while i < n do
    init <- Array.append init [|0|]
    i <- i + 1
init.[0] <- 1
let bytes: int array = evolve init 30
printfn "%s" (string bytes)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

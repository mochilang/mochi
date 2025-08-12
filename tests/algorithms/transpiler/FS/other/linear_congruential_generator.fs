// Generated 2025-08-12 09:13 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type LCG = {
    mutable _multiplier: int
    mutable _increment: int
    mutable _modulo: int
    mutable _seed: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System

let rec make_lcg (_multiplier: int) (_increment: int) (_modulo: int) (_seed: int) =
    let mutable __ret : LCG = Unchecked.defaultof<LCG>
    let mutable _multiplier = _multiplier
    let mutable _increment = _increment
    let mutable _modulo = _modulo
    let mutable _seed = _seed
    try
        __ret <- { _multiplier = _multiplier; _increment = _increment; _modulo = _modulo; _seed = _seed }
        raise Return
        __ret
    with
        | Return -> __ret
and next_number (lcg: LCG) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable lcg = lcg
    try
        lcg._seed <- (((((lcg._multiplier) * (lcg._seed)) + (lcg._increment)) % (lcg._modulo) + (lcg._modulo)) % (lcg._modulo))
        __ret <- lcg._seed
        raise Return
        __ret
    with
        | Return -> __ret
let mutable lcg: LCG = make_lcg (1664525) (1013904223) (int 4294967296L) (int (_now()))
let mutable i: int = 0
while i < 5 do
    ignore (printfn "%s" (_str (next_number (lcg))))
    i <- i + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

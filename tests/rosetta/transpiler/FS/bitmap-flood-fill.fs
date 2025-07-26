// Generated 2025-07-26 03:08 +0000

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
let mutable grid: string array array = [|[|"."; "."; "."; "."; "."|]; [|"."; "#"; "#"; "#"; "."|]; [|"."; "#"; "."; "#"; "."|]; [|"."; "#"; "#"; "#"; "."|]; [|"."; "."; "."; "."; "."|]|]
let rec flood (x: int) (y: int) (repl: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable x = x
    let mutable y = y
    let mutable repl = repl
    try
        let target: string = (grid.[y]).[x]
        if target = repl then
            __ret <- ()
            raise Return
        let rec ff (px: int) (py: int) =
            let mutable __ret = ()
            let mutable px = px
            let mutable py = py
            try
                if (((px < 0) || (py < 0)) || (py >= (unbox<int> (Array.length grid)))) || (px >= (Seq.length (grid.[0]))) then
                    __ret <- ()
                    raise Return
                if (unbox<string> ((grid.[py]).[px])) <> target then
                    __ret <- ()
                    raise Return
                (grid.[py]).[px] <- repl
                ff (px - 1) py
                ff (px + 1) py
                ff px (py - 1)
                ff px (py + 1)
                __ret
            with
                | Return -> __ret
        ff x y
        __ret
    with
        | Return -> __ret
flood 2 2 "o"
for row in grid do
    let mutable line: string = ""
    for ch in row do
        line <- line + (unbox<string> ch)
    printfn "%s" line
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

// Generated 2025-08-09 16:21 +0700

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
let rec move_tower (height: int) (from_pole: string) (to_pole: string) (with_pole: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable height = height
    let mutable from_pole = from_pole
    let mutable to_pole = to_pole
    let mutable with_pole = with_pole
    try
        if height >= 1 then
            move_tower (height - 1) (from_pole) (with_pole) (to_pole)
            move_disk (from_pole) (to_pole)
            move_tower (height - 1) (with_pole) (to_pole) (from_pole)
        __ret
    with
        | Return -> __ret
and move_disk (fp: string) (tp: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable fp = fp
    let mutable tp = tp
    try
        printfn "%s" ((("moving disk from " + fp) + " to ") + tp)
        __ret
    with
        | Return -> __ret
let height: int = 3
move_tower (height) ("A") ("B") ("C")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

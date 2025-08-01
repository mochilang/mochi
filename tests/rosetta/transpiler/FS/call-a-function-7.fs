// Generated 2025-07-27 10:08 +0000

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
let rec f () =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    try
        __ret <- unbox<obj array> [|box 0; box 0.0|]
        raise Return
        __ret
    with
        | Return -> __ret
and g (a: int) (b: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and h (s: string) (nums: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable s = s
    let mutable nums = nums
    try

        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        if (unbox<int> ((unbox<int> (2 * (unbox<int> (g 1 3.0)))) + 4)) > 0 then

        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

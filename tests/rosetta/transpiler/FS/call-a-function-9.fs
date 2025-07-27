// Generated 2025-07-27 17:18 +0700

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
        let ab: obj array = f()
        let a: obj = box (ab.[0])
        let b: obj = box (ab.[1])
        let cb: obj = (f()).[1]
        let d: int = g (unbox<int> a) (unbox<float> cb)
        let e: int = g d (unbox<float> b)
        let mutable i: int = g d 2.0
        let mutable list: int array = [||]
        list <- unbox<int array> (Array.append list [|unbox<int> a|])
        list <- unbox<int array> (Array.append list [|d|])
        list <- unbox<int array> (Array.append list [|e|])
        list <- unbox<int array> (Array.append list [|i|])
        i <- unbox<int> (Array.length list)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

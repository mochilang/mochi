// Generated 2025-07-26 05:05 +0700

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
let rec each (xs: int array) (f: int -> unit) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable xs = xs
    let mutable f = f
    try
        for x in xs do
            f x
        __ret
    with
        | Return -> __ret
and Map (xs: int array) (f: int -> int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable f = f
    try
        let mutable r: int array = [||]
        for x in xs do
            r <- Array.append r [|f x|]
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let s: int array = [|1; 2; 3; 4; 5|]
        each s (unbox<int -> unit> (        fun i -> (printfn "%s" (string (i * i)))))
        printfn "%s" (string (Map s (unbox<int -> int> (        fun i -> (i * i)))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

// Generated 2025-07-27 15:57 +0700

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
let rec mkAdd (a: int) =
    let mutable __ret : int -> int = Unchecked.defaultof<int -> int>
    let mutable a = a
    try
        __ret <- unbox<int -> int> (        fun b -> (a + b))
        raise Return
        __ret
    with
        | Return -> __ret
and mysum (x: int) (y: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    let mutable y = y
    try
        __ret <- x + y
        raise Return
        __ret
    with
        | Return -> __ret
and partialSum (x: int) =
    let mutable __ret : int -> int = Unchecked.defaultof<int -> int>
    let mutable x = x
    try
        __ret <- unbox<int -> int> (        fun y -> (mysum x y))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let add2: int -> int = mkAdd 2
        let add3: int -> int = mkAdd 3
        printfn "%s" (((string (add2 5)) + " ") + (string (add3 6)))
        let partial: int -> int = partialSum 13
        printfn "%s" (string (partial 5))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

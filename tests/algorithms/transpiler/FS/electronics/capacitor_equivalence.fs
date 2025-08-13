// Generated 2025-08-13 16:13 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec capacitor_parallel (capacitors: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable capacitors = capacitors
    try
        let mutable sum_c: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (capacitors)) do
            let c: float = _idx capacitors (int i)
            if c < 0.0 then
                ignore (failwith (("Capacitor at index " + (_str (i))) + " has a negative value!"))
                __ret <- 0.0
                raise Return
            sum_c <- sum_c + c
            i <- i + 1
        __ret <- sum_c
        raise Return
        __ret
    with
        | Return -> __ret
and capacitor_series (capacitors: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable capacitors = capacitors
    try
        let mutable first_sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (capacitors)) do
            let c: float = _idx capacitors (int i)
            if c <= 0.0 then
                ignore (failwith (("Capacitor at index " + (_str (i))) + " has a negative or zero value!"))
                __ret <- 0.0
                raise Return
            first_sum <- first_sum + (1.0 / c)
            i <- i + 1
        __ret <- 1.0 / first_sum
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let parallel: float = capacitor_parallel (unbox<float array> [|5.71389; 12.0; 3.0|])
        let series: float = capacitor_series (unbox<float array> [|5.71389; 12.0; 3.0|])
        ignore (printfn "%s" (_str (parallel)))
        ignore (printfn "%s" (_str (series)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

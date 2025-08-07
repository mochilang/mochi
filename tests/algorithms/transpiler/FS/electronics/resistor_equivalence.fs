// Generated 2025-08-07 15:46 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec resistor_parallel (resistors: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable resistors = resistors
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (resistors)) do
            let r: float = _idx resistors (i)
            if r <= 0.0 then
                failwith (("Resistor at index " + (_str (i))) + " has a negative or zero value!")
            sum <- sum + (1.0 / r)
            i <- i + 1
        __ret <- 1.0 / sum
        raise Return
        __ret
    with
        | Return -> __ret
and resistor_series (resistors: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable resistors = resistors
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (resistors)) do
            let r: float = _idx resistors (i)
            if r < 0.0 then
                failwith (("Resistor at index " + (_str (i))) + " has a negative value!")
            sum <- sum + r
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let resistors: float array = [|3.21389; 2.0; 3.0|]
        printfn "%s" ("Parallel: " + (_str (resistor_parallel (resistors))))
        printfn "%s" ("Series: " + (_str (resistor_series (resistors))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

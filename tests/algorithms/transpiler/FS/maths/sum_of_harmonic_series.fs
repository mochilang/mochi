// Generated 2025-08-08 18:58 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec sum_of_harmonic_progression (first_term: float) (common_difference: float) (number_of_terms: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable first_term = first_term
    let mutable common_difference = common_difference
    let mutable number_of_terms = number_of_terms
    try
        let mutable arithmetic_progression: float array = unbox<float array> [|1.0 / first_term|]
        let mutable term: float = 1.0 / first_term
        let mutable i: int = 0
        while i < (number_of_terms - 1) do
            term <- term + common_difference
            arithmetic_progression <- Array.append arithmetic_progression [|term|]
            i <- i + 1
        let mutable total: float = 0.0
        let mutable j: int = 0
        while j < (Seq.length (arithmetic_progression)) do
            total <- total + (1.0 / (_idx arithmetic_progression (int j)))
            j <- j + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and abs_val (num: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable num = num
    try
        __ret <- if num < 0.0 then (-num) else num
        raise Return
        __ret
    with
        | Return -> __ret
and test_sum_of_harmonic_progression () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let result1: float = sum_of_harmonic_progression (0.5) (2.0) (2)
        if (abs_val (result1 - 0.75)) > 0.0000001 then
            failwith ("test1 failed")
        let result2: float = sum_of_harmonic_progression (0.2) (5.0) (5)
        if (abs_val (result2 - 0.45666666666666667)) > 0.0000001 then
            failwith ("test2 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_sum_of_harmonic_progression()
        printfn "%g" (sum_of_harmonic_progression (0.5) (2.0) (2))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

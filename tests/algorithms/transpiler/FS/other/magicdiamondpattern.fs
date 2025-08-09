// Generated 2025-08-09 10:14 +0700

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
let rec floyd (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable result: string = ""
        let mutable i: int = 0
        while i < n do
            let mutable j: int = 0
            while j < ((n - i) - 1) do
                result <- result + " "
                j <- j + 1
            let mutable k: int = 0
            while k < (i + 1) do
                result <- result + "* "
                k <- k + 1
            result <- result + "\n"
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and reverse_floyd (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable result: string = ""
        let mutable i: int = n
        while i > 0 do
            let mutable j: int = i
            while j > 0 do
                result <- result + "* "
                j <- j - 1
            result <- result + "\n"
            let mutable k: int = (n - i) + 1
            while k > 0 do
                result <- result + " "
                k <- k - 1
            i <- i - 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and pretty_print (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n <= 0 then
            __ret <- "       ...       ....        nothing printing :("
            raise Return
        let upper_half: string = floyd (n)
        let lower_half: string = reverse_floyd (n)
        __ret <- upper_half + lower_half
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (pretty_print (3))
        printfn "%s" (pretty_print (0))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

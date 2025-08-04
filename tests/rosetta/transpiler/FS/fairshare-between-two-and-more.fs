// Generated 2025-08-04 20:44 +0700

exception Return
let mutable __ret = ()

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
let _padStart (s:string) (width:int) (pad:string) =
    let mutable out = s
    while out.Length < width do
        out <- pad + out
    out

let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
open System.Collections.Generic

let rec digitSumMod (n: int) (``base``: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    let mutable ``base`` = ``base``
    try
        let mutable sum: int = 0
        let mutable j: int = n
        while j > 0 do
            sum <- sum + (((j % ``base`` + ``base``) % ``base``))
            j <- j / ``base``
        __ret <- ((sum % ``base`` + ``base``) % ``base``)
        raise Return
        __ret
    with
        | Return -> __ret
and fairshareList (n: int) (``base``: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    let mutable ``base`` = ``base``
    try
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < n do
            res <- Array.append res [|digitSumMod (i) (``base``)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and sortInts (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable arr: int array = xs
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            let mutable j: int = 0
            while j < ((Seq.length (arr)) - 1) do
                if (_idx arr (j)) > (_idx arr (j + 1)) then
                    let t: int = _idx arr (j)
                    arr.[j] <- _idx arr (j + 1)
                    arr.[j + 1] <- t
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and turns (n: int) (``base``: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable ``base`` = ``base``
    try
        let mutable counts: int array = [||]
        let mutable i: int = 0
        while i < ``base`` do
            counts <- Array.append counts [|0|]
            i <- i + 1
        i <- 0
        while i < n do
            let v: int = digitSumMod (i) (``base``)
            counts.[v] <- (_idx counts (v)) + 1
            i <- i + 1
        let mutable freq: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        let mutable fkeys: int array = [||]
        i <- 0
        while i < ``base`` do
            let c: int = _idx counts (i)
            if c > 0 then
                if freq.ContainsKey(c) then
                    freq.[c] <- (freq.[c]) + 1
                else
                    freq.[c] <- 1
                    fkeys <- Array.append fkeys [|c|]
            i <- i + 1
        let mutable total: int = 0
        i <- 0
        while i < (Seq.length (fkeys)) do
            total <- total + (freq.[_idx fkeys (i)])
            i <- i + 1
        if total <> ``base`` then
            __ret <- ("only " + (string (total))) + " have a turn"
            raise Return
        fkeys <- sortInts (fkeys)
        let mutable res: string = ""
        i <- 0
        while i < (Seq.length (fkeys)) do
            if i > 0 then
                res <- res + " or "
            res <- res + (string (_idx fkeys (i)))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable bases1: int array = [|2; 3; 5; 11|]
        let mutable i: int = 0
        while i < (Seq.length (bases1)) do
            let b: int = _idx bases1 (i)
            printfn "%s" (((unbox<string> (_padStart (string (b)) (2) (" "))) + " : ") + (("[" + (unbox<string> (String.concat (" ") (Array.toList (Array.map string (fairshareList (25) (b))))))) + "]"))
            i <- i + 1
        printfn "%s" ("")
        printfn "%s" ("How many times does each get a turn in 50000 iterations?")
        let mutable bases2: int array = [|191; 1377; 49999; 50000; 50001|]
        i <- 0
        while i < (Seq.length (bases2)) do
            let b: int = _idx bases2 (i)
            let t: string = turns (50000) (b)
            printfn "%s" ((("  With " + (string (b))) + " people: ") + t)
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

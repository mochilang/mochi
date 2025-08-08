// Generated 2025-08-08 18:09 +0700

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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let mutable lcg_seed: int = 1
let rec lcg_rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        lcg_seed <- int ((((((int64 lcg_seed) * (int64 1103515245)) + (int64 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- lcg_seed
        raise Return
        __ret
    with
        | Return -> __ret
and roll () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        let rv: float = float (lcg_rand())
        let r: float = (rv * 6.0) / 2147483648.0
        __ret <- 1 + (int r)
        raise Return
        __ret
    with
        | Return -> __ret
and round2 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let y: float = (x * 100.0) + 0.5
        let z: int = int y
        __ret <- (float z) / 100.0
        raise Return
        __ret
    with
        | Return -> __ret
and throw_dice (num_throws: int) (num_dice: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable num_throws = num_throws
    let mutable num_dice = num_dice
    try
        let mutable count_of_sum: int array = Array.empty<int>
        let max_sum: int64 = ((int64 num_dice) * (int64 6)) + (int64 1)
        let mutable i: int = 0
        while (int64 i) < max_sum do
            count_of_sum <- Array.append count_of_sum [|0|]
            i <- i + 1
        let mutable t: int = 0
        while t < num_throws do
            let mutable s: int = 0
            let mutable d: int = 0
            while d < num_dice do
                s <- s + (roll())
                d <- d + 1
            count_of_sum.[s] <- (_idx count_of_sum (s)) + 1
            t <- t + 1
        let mutable probability: float array = Array.empty<float>
        i <- num_dice
        while (int64 i) < max_sum do
            let p: float = ((float (_idx count_of_sum (i))) * 100.0) / (float num_throws)
            probability <- Array.append probability [|(round2 (p))|]
            i <- i + 1
        __ret <- probability
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        lcg_seed <- 1
        let result: float array = throw_dice (10000) (2)
        printfn "%s" (_str (result))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let mutable _seed: int = 1
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        _seed <- int ((((((int64 _seed) * (int64 1103515245)) + (int64 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- _floordiv _seed 65536
        raise Return
        __ret
    with
        | Return -> __ret
let rec randint (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let r: int = rand()
        __ret <- a + (((r % ((b - a) + 1) + ((b - a) + 1)) % ((b - a) + 1)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec fisher_yates_shuffle_int (data: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable data = data
    try
        let mutable res: int array = data
        let mutable i: int = 0
        while i < (Seq.length (res)) do
            let a: int = randint (0) ((Seq.length (res)) - 1)
            let b: int = randint (0) ((Seq.length (res)) - 1)
            let temp: int = _idx res (int a)
            res.[int a] <- _idx res (int b)
            res.[int b] <- temp
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec fisher_yates_shuffle_str (data: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable data = data
    try
        let mutable res: string array = data
        let mutable i: int = 0
        while i < (Seq.length (res)) do
            let a: int = randint (0) ((Seq.length (res)) - 1)
            let b: int = randint (0) ((Seq.length (res)) - 1)
            let temp: string = _idx res (int a)
            res.[int a] <- _idx res (int b)
            res.[int b] <- temp
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let integers: int array = unbox<int array> [|0; 1; 2; 3; 4; 5; 6; 7|]
let strings: string array = unbox<string array> [|"python"; "says"; "hello"; "!"|]
printfn "%s" ("Fisher-Yates Shuffle:")
printfn "%s" ((("List " + (_str (integers))) + " ") + (_str (strings)))
printfn "%s" ((("FY Shuffle " + (_str (fisher_yates_shuffle_int (integers)))) + " ") + (_str (fisher_yates_shuffle_str (strings))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

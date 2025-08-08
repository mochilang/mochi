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
let rec sock_merchant (colors: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable colors = colors
    try
        let mutable arr: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (colors)) do
            arr <- Array.append arr [|(_idx colors (int i))|]
            i <- i + 1
        let mutable n: int = Seq.length (arr)
        let mutable a: int = 0
        while a < n do
            let mutable min_idx: int = a
            let mutable b: int = a + 1
            while b < n do
                if (_idx arr (int b)) < (_idx arr (int min_idx)) then
                    min_idx <- b
                b <- b + 1
            let temp: int = _idx arr (int a)
            arr.[int a] <- _idx arr (int min_idx)
            arr.[int min_idx] <- temp
            a <- a + 1
        let mutable pairs: int = 0
        i <- 0
        while i < n do
            let mutable count: int = 1
            while ((i + 1) < n) && ((_idx arr (int i)) = (_idx arr (int (i + 1)))) do
                count <- count + 1
                i <- i + 1
            pairs <- pairs + (_floordiv count 2)
            i <- i + 1
        __ret <- pairs
        raise Return
        __ret
    with
        | Return -> __ret
and test_sock_merchant () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let example1: int array = unbox<int array> [|10; 20; 20; 10; 10; 30; 50; 10; 20|]
        if (sock_merchant (example1)) <> 3 then
            failwith ("example1 failed")
        let example2: int array = unbox<int array> [|1; 1; 3; 3|]
        if (sock_merchant (example2)) <> 2 then
            failwith ("example2 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_sock_merchant()
        let example1: int array = unbox<int array> [|10; 20; 20; 10; 10; 30; 50; 10; 20|]
        printfn "%s" (_str (sock_merchant (example1)))
        let example2: int array = unbox<int array> [|1; 1; 3; 3|]
        printfn "%s" (_str (sock_merchant (example2)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

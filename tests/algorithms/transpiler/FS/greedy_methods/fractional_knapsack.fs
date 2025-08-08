// Generated 2025-08-08 16:03 +0700

exception Break
exception Continue

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
type Item = {
    mutable _value: float
    mutable _weight: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sort_by_ratio_desc (arr: Item array) =
    let mutable __ret : Item array = Unchecked.defaultof<Item array>
    let mutable arr = arr
    try
        let mutable i: int = 1
        try
            while i < (Seq.length (arr)) do
                try
                    let key: Item = _idx arr (i)
                    let mutable j: int = i - 1
                    try
                        while j >= 0 do
                            try
                                let current: Item = _idx arr (j)
                                if ((current._value) / (current._weight)) < ((key._value) / (key._weight)) then
                                    arr.[j + 1] <- current
                                    j <- j - 1
                                else
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    arr.[j + 1] <- key
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec sum_first (arr: float array) (k: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable arr = arr
    let mutable k = k
    try
        let mutable s: float = 0.0
        let mutable i: int = 0
        while (i < k) && (i < (Seq.length (arr))) do
            s <- s + (_idx arr (i))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec frac_knapsack (vl: float array) (wt: float array) (w: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable vl = vl
    let mutable wt = wt
    let mutable w = w
    let mutable n = n
    try
        let mutable items: Item array = [||]
        let mutable i: int = 0
        while (i < (Seq.length (vl))) && (i < (Seq.length (wt))) do
            items <- Array.append items [|{ _value = _idx vl (i); _weight = _idx wt (i) }|]
            i <- i + 1
        items <- sort_by_ratio_desc (items)
        let mutable values: float array = [||]
        let mutable weights: float array = [||]
        i <- 0
        while i < (Seq.length (items)) do
            let itm: Item = _idx items (i)
            values <- Array.append values [|(itm._value)|]
            weights <- Array.append weights [|(itm._weight)|]
            i <- i + 1
        let mutable acc: float array = [||]
        let mutable total: float = 0.0
        i <- 0
        while i < (Seq.length (weights)) do
            total <- total + (_idx weights (i))
            acc <- Array.append acc [|total|]
            i <- i + 1
        let mutable k: int = 0
        while (k < (Seq.length (acc))) && (w >= (_idx acc (k))) do
            k <- k + 1
        if k = 0 then
            __ret <- 0.0
            raise Return
        if k >= (Seq.length (values)) then
            __ret <- sum_first (values) (Seq.length (values))
            raise Return
        if k <> n then
            __ret <- (sum_first (values) (k)) + (((w - (_idx acc (k - 1))) * (_idx values (k))) / (_idx weights (k)))
            raise Return
        __ret <- sum_first (values) (k)
        raise Return
        __ret
    with
        | Return -> __ret
let vl: float array = [|60.0; 100.0; 120.0|]
let wt: float array = [|10.0; 20.0; 30.0|]
let result: float = frac_knapsack (vl) (wt) (50.0) (3)
printfn "%s" (_str (result))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

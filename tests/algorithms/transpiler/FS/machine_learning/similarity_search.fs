// Generated 2025-08-08 17:07 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Neighbor = {
    mutable _vector: float array
    mutable _distance: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sqrt (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 10 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec euclidean (a: float array) (b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            let diff: float = (_idx a (i)) - (_idx b (i))
            sum <- sum + (diff * diff)
            i <- i + 1
        let res: float = sqrt (sum)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec similarity_search (dataset: float array array) (value_array: float array array) =
    let mutable __ret : Neighbor array = Unchecked.defaultof<Neighbor array>
    let mutable dataset = dataset
    let mutable value_array = value_array
    try
        let dim: int = Seq.length (_idx dataset (0))
        if dim <> (Seq.length (_idx value_array (0))) then
            __ret <- Array.empty<Neighbor>
            raise Return
        let mutable result: Neighbor array = Array.empty<Neighbor>
        let mutable i: int = 0
        while i < (Seq.length (value_array)) do
            let value: float array = _idx value_array (i)
            let mutable dist: float = euclidean (value) (_idx dataset (0))
            let mutable vec: float array = _idx dataset (0)
            let mutable j: int = 1
            while j < (Seq.length (dataset)) do
                let d: float = euclidean (value) (_idx dataset (j))
                if d < dist then
                    dist <- d
                    vec <- _idx dataset (j)
                j <- j + 1
            let nb: Neighbor = { _vector = vec; _distance = dist }
            result <- Array.append result [|nb|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec cosine_similarity (a: float array) (b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let mutable dot: float = 0.0
        let mutable norm_a: float = 0.0
        let mutable norm_b: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            dot <- dot + ((_idx a (i)) * (_idx b (i)))
            norm_a <- norm_a + ((_idx a (i)) * (_idx a (i)))
            norm_b <- norm_b + ((_idx b (i)) * (_idx b (i)))
            i <- i + 1
        if (norm_a = 0.0) || (norm_b = 0.0) then
            __ret <- 0.0
            raise Return
        __ret <- dot / ((sqrt (norm_a)) * (sqrt (norm_b)))
        raise Return
        __ret
    with
        | Return -> __ret
let dataset: float array array = [|[|0.0; 0.0; 0.0|]; [|1.0; 1.0; 1.0|]; [|2.0; 2.0; 2.0|]|]
let value_array: float array array = [|[|0.0; 0.0; 0.0|]; [|0.0; 0.0; 1.0|]|]
let neighbors: Neighbor array = similarity_search (dataset) (value_array)
let mutable k: int = 0
while k < (Seq.length (neighbors)) do
    let n: Neighbor = _idx neighbors (k)
    printfn "%s" (((("[" + (_str (n._vector))) + ", ") + (_str (n._distance))) + "]")
    k <- k + 1
printfn "%s" (_str (cosine_similarity (unbox<float array> [|1.0; 2.0|]) (unbox<float array> [|6.0; 32.0|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

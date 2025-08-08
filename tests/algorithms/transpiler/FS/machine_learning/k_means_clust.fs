// Generated 2025-08-08 17:07 +0700

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
type KMeansResult = {
    mutable _centroids: float array array
    mutable _assignments: int array
    mutable _heterogeneity: float array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec distance_sq (a: float array) (b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let mutable sum: float = 0.0
        for i in 0 .. ((Seq.length (a)) - 1) do
            let diff: float = (_idx a (i)) - (_idx b (i))
            sum <- sum + (diff * diff)
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec assign_clusters (data: float array array) (_centroids: float array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable data = data
    let mutable _centroids = _centroids
    try
        let mutable _assignments: int array = Array.empty<int>
        for i in 0 .. ((Seq.length (data)) - 1) do
            let mutable best_idx: int = 0
            let mutable best: float = distance_sq (_idx data (i)) (_idx _centroids (0))
            for j in 1 .. ((Seq.length (_centroids)) - 1) do
                let dist: float = distance_sq (_idx data (i)) (_idx _centroids (j))
                if dist < best then
                    best <- dist
                    best_idx <- j
            _assignments <- Array.append _assignments [|best_idx|]
        __ret <- _assignments
        raise Return
        __ret
    with
        | Return -> __ret
let rec revise_centroids (data: float array array) (k: int) (assignment: int array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable data = data
    let mutable k = k
    let mutable assignment = assignment
    try
        let dim: int = Seq.length (_idx data (0))
        let mutable sums: float array array = Array.empty<float array>
        let mutable counts: int array = Array.empty<int>
        for i in 0 .. (k - 1) do
            let mutable row: float array = Array.empty<float>
            for j in 0 .. (dim - 1) do
                row <- Array.append row [|0.0|]
            sums <- Array.append sums [|row|]
            counts <- Array.append counts [|0|]
        for i in 0 .. ((Seq.length (data)) - 1) do
            let c: int = _idx assignment (i)
            counts.[c] <- (_idx counts (c)) + 1
            for j in 0 .. (dim - 1) do
                sums.[c].[j] <- (_idx (_idx sums (c)) (j)) + (_idx (_idx data (i)) (j))
        let mutable _centroids: float array array = Array.empty<float array>
        for i in 0 .. (k - 1) do
            let mutable row: float array = Array.empty<float>
            if (_idx counts (i)) > 0 then
                for j in 0 .. (dim - 1) do
                    row <- Array.append row [|((_idx (_idx sums (i)) (j)) / (float (_idx counts (i))))|]
            else
                for j in 0 .. (dim - 1) do
                    row <- Array.append row [|0.0|]
            _centroids <- Array.append _centroids [|row|]
        __ret <- _centroids
        raise Return
        __ret
    with
        | Return -> __ret
let rec compute_heterogeneity (data: float array array) (_centroids: float array array) (assignment: int array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable data = data
    let mutable _centroids = _centroids
    let mutable assignment = assignment
    try
        let mutable total: float = 0.0
        for i in 0 .. ((Seq.length (data)) - 1) do
            let c: int = _idx assignment (i)
            total <- total + (distance_sq (_idx data (i)) (_idx _centroids (c)))
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
let rec lists_equal (a: int array) (b: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        if (Seq.length (a)) <> (Seq.length (b)) then
            __ret <- false
            raise Return
        for i in 0 .. ((Seq.length (a)) - 1) do
            if (_idx a (i)) <> (_idx b (i)) then
                __ret <- false
                raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec kmeans (data: float array array) (k: int) (initial_centroids: float array array) (max_iter: int) =
    let mutable __ret : KMeansResult = Unchecked.defaultof<KMeansResult>
    let mutable data = data
    let mutable k = k
    let mutable initial_centroids = initial_centroids
    let mutable max_iter = max_iter
    try
        let mutable _centroids: float array array = initial_centroids
        let mutable assignment: int array = Array.empty<int>
        let mutable prev: int array = Array.empty<int>
        let mutable _heterogeneity: float array = Array.empty<float>
        let mutable iter: int = 0
        try
            while iter < max_iter do
                try
                    assignment <- assign_clusters (data) (_centroids)
                    _centroids <- revise_centroids (data) (k) (assignment)
                    let h: float = compute_heterogeneity (data) (_centroids) (assignment)
                    _heterogeneity <- Array.append _heterogeneity [|h|]
                    if (iter > 0) && (lists_equal (prev) (assignment)) then
                        raise Break
                    prev <- assignment
                    iter <- iter + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- { _centroids = _centroids; _assignments = assignment; _heterogeneity = _heterogeneity }
        raise Return
        __ret
    with
        | Return -> __ret
let data: float array array = [|[|1.0; 2.0|]; [|1.5; 1.8|]; [|5.0; 8.0|]; [|8.0; 8.0|]; [|1.0; 0.6|]; [|9.0; 11.0|]|]
let k: int = 3
let initial_centroids: float array array = [|_idx data (0); _idx data (2); _idx data (5)|]
let result: KMeansResult = kmeans (data) (k) (initial_centroids) (10)
printfn "%s" (_str (result._centroids))
printfn "%s" (_str (result._assignments))
printfn "%s" (_str (result._heterogeneity))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

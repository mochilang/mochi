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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
type PointLabel = {
    mutable _point: float array
    mutable label: int
}
type KNN = {
    mutable _data: PointLabel array
    mutable _labels: string array
}
type DistLabel = {
    mutable _dist: float
    mutable label: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec make_knn (train_data: float array array) (train_target: int array) (class_labels: string array) =
    let mutable __ret : KNN = Unchecked.defaultof<KNN>
    let mutable train_data = train_data
    let mutable train_target = train_target
    let mutable class_labels = class_labels
    try
        let mutable items: PointLabel array = Array.empty<PointLabel>
        let mutable i: int = 0
        while i < (Seq.length (train_data)) do
            let pl: PointLabel = { _point = _idx train_data (i); label = _idx train_target (i) }
            items <- Array.append items [|pl|]
            i <- i + 1
        __ret <- { _data = items; _labels = class_labels }
        raise Return
        __ret
    with
        | Return -> __ret
let rec euclidean_distance (a: float array) (b: float array) =
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
        __ret <- sqrtApprox (sum)
        raise Return
        __ret
    with
        | Return -> __ret
let rec classify (knn: KNN) (pred_point: float array) (k: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable knn = knn
    let mutable pred_point = pred_point
    let mutable k = k
    try
        let mutable distances: DistLabel array = Array.empty<DistLabel>
        let mutable i: int = 0
        while i < (Seq.length (knn._data)) do
            let d: float = euclidean_distance ((_idx (knn._data) (i))._point) (pred_point)
            distances <- Array.append distances [|{ _dist = d; label = (_idx (knn._data) (i)).label }|]
            i <- i + 1
        let mutable votes: int array = Array.empty<int>
        let mutable count: int = 0
        while count < k do
            let mutable min_index: int = 0
            let mutable j: int = 1
            while j < (Seq.length (distances)) do
                if ((_idx distances (j))._dist) < ((_idx distances (min_index))._dist) then
                    min_index <- j
                j <- j + 1
            votes <- Array.append votes [|((_idx distances (min_index)).label)|]
            distances.[min_index]._dist <- 1000000000000000000.0
            count <- count + 1
        let mutable tally: int array = Array.empty<int>
        let mutable t: int = 0
        while t < (Seq.length (knn._labels)) do
            tally <- Array.append tally [|0|]
            t <- t + 1
        let mutable v: int = 0
        while v < (Seq.length (votes)) do
            let lbl: int = _idx votes (v)
            tally.[lbl] <- (_idx tally (lbl)) + 1
            v <- v + 1
        let mutable max_idx: int = 0
        let mutable m: int = 1
        while m < (Seq.length (tally)) do
            if (_idx tally (m)) > (_idx tally (max_idx)) then
                max_idx <- m
            m <- m + 1
        __ret <- _idx (knn._labels) (max_idx)
        raise Return
        __ret
    with
        | Return -> __ret
let train_X: float array array = [|[|0.0; 0.0|]; [|1.0; 0.0|]; [|0.0; 1.0|]; [|0.5; 0.5|]; [|3.0; 3.0|]; [|2.0; 3.0|]; [|3.0; 2.0|]|]
let train_y: int array = unbox<int array> [|0; 0; 0; 0; 1; 1; 1|]
let classes: string array = unbox<string array> [|"A"; "B"|]
let knn: KNN = make_knn (train_X) (train_y) (classes)
let _point: float array = unbox<float array> [|1.2; 1.2|]
printfn "%s" (classify (knn) (_point) (5))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

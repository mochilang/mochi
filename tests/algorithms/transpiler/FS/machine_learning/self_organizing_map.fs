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
let rec get_winner (weights: float array array) (sample: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable weights = weights
    let mutable sample = sample
    try
        let mutable d0: float = 0.0
        let mutable d1: float = 0.0
        for i in 0 .. ((Seq.length (sample)) - 1) do
            let diff0: float = (float (_idx sample (i))) - (_idx (_idx weights (0)) (i))
            let diff1: float = (float (_idx sample (i))) - (_idx (_idx weights (1)) (i))
            d0 <- d0 + (diff0 * diff0)
            d1 <- d1 + (diff1 * diff1)
            __ret <- if d0 > d1 then 0 else 1
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and update (weights: float array array) (sample: int array) (j: int) (alpha: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable weights = weights
    let mutable sample = sample
    let mutable j = j
    let mutable alpha = alpha
    try
        for i in 0 .. ((Seq.length (weights)) - 1) do
            weights.[j].[i] <- (_idx (_idx weights (j)) (i)) + (alpha * ((float (_idx sample (i))) - (_idx (_idx weights (j)) (i))))
        __ret <- weights
        raise Return
        __ret
    with
        | Return -> __ret
and list_to_string (xs: float array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            s <- s + (_str (_idx xs (i)))
            if i < ((Seq.length (xs)) - 1) then
                s <- s + ", "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_to_string (m: float array array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable m = m
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (m)) do
            s <- s + (list_to_string (_idx m (i)))
            if i < ((Seq.length (m)) - 1) then
                s <- s + ", "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let training_samples: int array array = [|[|1; 1; 0; 0|]; [|0; 0; 0; 1|]; [|1; 0; 0; 0|]; [|0; 0; 1; 1|]|]
        let mutable weights: float array array = [|[|0.2; 0.6; 0.5; 0.9|]; [|0.8; 0.4; 0.7; 0.3|]|]
        let epochs: int = 3
        let alpha: float = 0.5
        for _ in 0 .. (epochs - 1) do
            for j in 0 .. ((Seq.length (training_samples)) - 1) do
                let sample: int array = _idx training_samples (j)
                let winner: int = get_winner (weights) (sample)
                weights <- update (weights) (sample) (winner) (alpha)
        let sample: int array = unbox<int array> [|0; 0; 0; 1|]
        let winner: int = get_winner (weights) (sample)
        printfn "%s" ("Clusters that the test sample belongs to : " + (_str (winner)))
        printfn "%s" ("Weights that have been trained : " + (matrix_to_string (weights)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

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
type Tree =
    | Leaf of float
    | Branch of float * Tree * Tree
let PI: float = 3.141592653589793
let TWO_PI: float = 6.283185307179586
let rec _mod (x: float) (m: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable m = m
    try
        __ret <- x - ((float (int (x / m))) * m)
        raise Return
        __ret
    with
        | Return -> __ret
and sin (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable y: float = (_mod (x + PI) (TWO_PI)) - PI
        let y2: float = y * y
        let y3: float = y2 * y
        let y5: float = y3 * y2
        let y7: float = y5 * y2
        __ret <- ((y - (y3 / 6.0)) + (y5 / 120.0)) - (y7 / 5040.0)
        raise Return
        __ret
    with
        | Return -> __ret
let mutable _seed: int = 123456789
let rec rand () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        _seed <- int ((((int64 ((1103515245 * _seed) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- (float _seed) / 2147483648.0
        raise Return
        __ret
    with
        | Return -> __ret
and mean (vals: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable vals = vals
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (vals)) do
            sum <- sum + (_idx vals (i))
            i <- i + 1
        __ret <- sum / (float (Seq.length (vals)))
        raise Return
        __ret
    with
        | Return -> __ret
and mean_squared_error (labels: float array) (prediction: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable labels = labels
    let mutable prediction = prediction
    try
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (labels)) do
            let diff: float = (_idx labels (i)) - prediction
            total <- total + (diff * diff)
            i <- i + 1
        __ret <- total / (float (Seq.length (labels)))
        raise Return
        __ret
    with
        | Return -> __ret
and train_tree (x: float array) (y: float array) (depth: int) (min_leaf_size: int) =
    let mutable __ret : Tree = Unchecked.defaultof<Tree>
    let mutable x = x
    let mutable y = y
    let mutable depth = depth
    let mutable min_leaf_size = min_leaf_size
    try
        if (Seq.length (x)) < (2 * min_leaf_size) then
            __ret <- Leaf(mean (y))
            raise Return
        if depth = 1 then
            __ret <- Leaf(mean (y))
            raise Return
        let mutable best_split: int = 0
        let mutable min_error: float = (mean_squared_error (x) (mean (y))) * 2.0
        let mutable i: int = 0
        while i < (Seq.length (x)) do
            if (Seq.length (Array.sub x 0 (i - 0))) < min_leaf_size then
                i <- i
            else
                if (Seq.length (Array.sub x i ((int (Array.length (x))) - i))) < min_leaf_size then
                    i <- i
                else
                    let err_left: float = mean_squared_error (Array.sub x 0 (i - 0)) (mean (Array.sub y 0 (i - 0)))
                    let err_right: float = mean_squared_error (Array.sub x i ((int (Array.length (x))) - i)) (mean (Array.sub y i ((int (Array.length (y))) - i)))
                    let err: float = err_left + err_right
                    if err < min_error then
                        best_split <- i
                        min_error <- err
            i <- i + 1
        if best_split <> 0 then
            let left_x: float array = Array.sub x 0 (best_split - 0)
            let left_y: float array = Array.sub y 0 (best_split - 0)
            let right_x: float array = Array.sub x best_split ((int (Array.length (x))) - best_split)
            let right_y: float array = Array.sub y best_split ((int (Array.length (y))) - best_split)
            let boundary: float = _idx x (best_split)
            let left_tree: Tree = train_tree (left_x) (left_y) (depth - 1) (min_leaf_size)
            let right_tree: Tree = train_tree (right_x) (right_y) (depth - 1) (min_leaf_size)
            __ret <- Branch(boundary, left_tree, right_tree)
            raise Return
        __ret <- Leaf(mean (y))
        raise Return
        __ret
    with
        | Return -> __ret
and predict (tree: Tree) (value: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable tree = tree
    let mutable value = value
    try
        __ret <- float ((match tree with
            | Leaf(p) -> p
            | Branch(b, l, r) -> if value >= (float b) then (predict (r) (value)) else (predict (l) (value))))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable x: float array = Array.empty<float>
        let mutable v: float = -1.0
        while v < 1.0 do
            x <- Array.append x [|v|]
            v <- v + 0.005
        let mutable y: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (x)) do
            y <- Array.append y [|(sin (_idx x (i)))|]
            i <- i + 1
        let tree: Tree = train_tree (x) (y) (10) (10)
        let mutable test_cases: float array = Array.empty<float>
        i <- 0
        while i < 10 do
            test_cases <- Array.append test_cases [|(((rand()) * 2.0) - 1.0)|]
            i <- i + 1
        let mutable predictions: float array = Array.empty<float>
        i <- 0
        while i < (Seq.length (test_cases)) do
            predictions <- Array.append predictions [|(predict (tree) (_idx test_cases (i)))|]
            i <- i + 1
        let mutable sum_err: float = 0.0
        i <- 0
        while i < (Seq.length (test_cases)) do
            let diff: float = (_idx predictions (i)) - (_idx test_cases (i))
            sum_err <- sum_err + (diff * diff)
            i <- i + 1
        let avg_error: float = sum_err / (float (Seq.length (test_cases)))
        printfn "%s" ("Test values: " + (_str (test_cases)))
        printfn "%s" ("Predictions: " + (_str (predictions)))
        printfn "%s" ("Average error: " + (_str (avg_error)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

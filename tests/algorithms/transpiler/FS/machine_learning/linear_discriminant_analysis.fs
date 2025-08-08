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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let PI: float = 3.141592653589793
let TWO_PI: float = 6.283185307179586
let mutable _seed: int = 1
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        _seed <- int ((((int64 ((_seed * 1103515245) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- _seed
        raise Return
        __ret
    with
        | Return -> __ret
and random () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        __ret <- (float (rand())) / 2147483648.0
        raise Return
        __ret
    with
        | Return -> __ret
and _mod (x: float) (m: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable m = m
    try
        __ret <- x - ((float (int (x / m))) * m)
        raise Return
        __ret
    with
        | Return -> __ret
and cos (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let y: float = (_mod (x + PI) (TWO_PI)) - PI
        let y2: float = y * y
        let y4: float = y2 * y2
        let y6: float = y4 * y2
        __ret <- ((1.0 - (y2 / 2.0)) + (y4 / 24.0)) - (y6 / 720.0)
        raise Return
        __ret
    with
        | Return -> __ret
and sqrtApprox (x: float) =
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
and ln (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable t: float = (x - 1.0) / (x + 1.0)
        let mutable term: float = t
        let mutable sum: float = 0.0
        let mutable n: int = 1
        while n <= 19 do
            sum <- sum + (term / (float n))
            term <- (term * t) * t
            n <- n + 2
        __ret <- 2.0 * sum
        raise Return
        __ret
    with
        | Return -> __ret
and gaussian_distribution (mean: float) (std_dev: float) (instance_count: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable mean = mean
    let mutable std_dev = std_dev
    let mutable instance_count = instance_count
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < instance_count do
            let u1: float = random()
            let u2: float = random()
            let r: float = sqrtApprox ((-2.0) * (ln (u1)))
            let theta: float = TWO_PI * u2
            let z: float = r * (cos (theta))
            res <- Array.append res [|(mean + (z * std_dev))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and y_generator (class_count: int) (instance_count: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable class_count = class_count
    let mutable instance_count = instance_count
    try
        let mutable res: int array = Array.empty<int>
        let mutable k: int = 0
        while k < class_count do
            let mutable i: int = 0
            while i < (_idx instance_count (k)) do
                res <- Array.append res [|k|]
                i <- i + 1
            k <- k + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and calculate_mean (instance_count: int) (items: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable instance_count = instance_count
    let mutable items = items
    try
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < instance_count do
            total <- total + (_idx items (i))
            i <- i + 1
        __ret <- total / (float instance_count)
        raise Return
        __ret
    with
        | Return -> __ret
and calculate_probabilities (instance_count: int) (total_count: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable instance_count = instance_count
    let mutable total_count = total_count
    try
        __ret <- (float instance_count) / (float total_count)
        raise Return
        __ret
    with
        | Return -> __ret
and calculate_variance (items: float array array) (means: float array) (total_count: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable items = items
    let mutable means = means
    let mutable total_count = total_count
    try
        let mutable squared_diff: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (items)) do
            let mutable j: int = 0
            while j < (Seq.length (_idx items (i))) do
                let diff: float = (_idx (_idx items (i)) (j)) - (_idx means (i))
                squared_diff <- Array.append squared_diff [|(diff * diff)|]
                j <- j + 1
            i <- i + 1
        let mutable sum_sq: float = 0.0
        let mutable k: int = 0
        while k < (Seq.length (squared_diff)) do
            sum_sq <- sum_sq + (_idx squared_diff (k))
            k <- k + 1
        let n_classes: int = Seq.length (means)
        __ret <- (1.0 / (float (total_count - n_classes))) * sum_sq
        raise Return
        __ret
    with
        | Return -> __ret
and predict_y_values (x_items: float array array) (means: float array) (variance: float) (probabilities: float array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable x_items = x_items
    let mutable means = means
    let mutable variance = variance
    let mutable probabilities = probabilities
    try
        let mutable results: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (x_items)) do
            let mutable j: int = 0
            while j < (Seq.length (_idx x_items (i))) do
                let mutable temp: float array = Array.empty<float>
                let mutable k: int = 0
                while k < (Seq.length (x_items)) do
                    let discr: float = (((_idx (_idx x_items (i)) (j)) * ((_idx means (k)) / variance)) - (((_idx means (k)) * (_idx means (k))) / (2.0 * variance))) + (ln (_idx probabilities (k)))
                    temp <- Array.append temp [|discr|]
                    k <- k + 1
                let mutable max_idx: int = 0
                let mutable max_val: float = _idx temp (0)
                let mutable t: int = 1
                while t < (Seq.length (temp)) do
                    if (_idx temp (t)) > max_val then
                        max_val <- _idx temp (t)
                        max_idx <- t
                    t <- t + 1
                results <- Array.append results [|max_idx|]
                j <- j + 1
            i <- i + 1
        __ret <- results
        raise Return
        __ret
    with
        | Return -> __ret
and accuracy (actual_y: int array) (predicted_y: int array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable actual_y = actual_y
    let mutable predicted_y = predicted_y
    try
        let mutable correct: int = 0
        let mutable i: int = 0
        while i < (Seq.length (actual_y)) do
            if (_idx actual_y (i)) = (_idx predicted_y (i)) then
                correct <- correct + 1
            i <- i + 1
        __ret <- ((float correct) / (float (Seq.length (actual_y)))) * 100.0
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        _seed <- 1
        let counts: int array = unbox<int array> [|20; 20; 20|]
        let means: float array = unbox<float array> [|5.0; 10.0; 15.0|]
        let std_dev: float = 1.0
        let mutable x: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (Seq.length (counts)) do
            x <- Array.append x [|(gaussian_distribution (_idx means (i)) (std_dev) (_idx counts (i)))|]
            i <- i + 1
        let y: int array = y_generator (Seq.length (counts)) (counts)
        let mutable actual_means: float array = Array.empty<float>
        i <- 0
        while i < (Seq.length (counts)) do
            actual_means <- Array.append actual_means [|(calculate_mean (_idx counts (i)) (_idx x (i)))|]
            i <- i + 1
        let mutable total_count: int = 0
        i <- 0
        while i < (Seq.length (counts)) do
            total_count <- total_count + (_idx counts (i))
            i <- i + 1
        let mutable probabilities: float array = Array.empty<float>
        i <- 0
        while i < (Seq.length (counts)) do
            probabilities <- Array.append probabilities [|(calculate_probabilities (_idx counts (i)) (total_count))|]
            i <- i + 1
        let variance: float = calculate_variance (x) (actual_means) (total_count)
        let predicted: int array = predict_y_values (x) (actual_means) (variance) (probabilities)
        printfn "%s" (_repr (predicted))
        printfn "%g" (accuracy (y) (predicted))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

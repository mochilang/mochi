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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
type PCAResult = {
    mutable _transformed: float array array
    mutable _variance_ratio: float array
}
type Eigen = {
    mutable _values: float array
    mutable _vectors: float array array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sqrt (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable guess: float = if x > 1.0 then (x / 2.0) else 1.0
        let mutable i: int = 0
        while i < 20 do
            guess <- 0.5 * (guess + (x / guess))
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec mean (xs: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable xs = xs
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            sum <- sum + (_idx xs (i))
            i <- i + 1
        __ret <- sum / (float (Seq.length (xs)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec standardize (data: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable data = data
    try
        let n_samples: int = Seq.length (data)
        let n_features: int = Seq.length (_idx data (0))
        let mutable means: float array = Array.empty<float>
        let mutable stds: float array = Array.empty<float>
        let mutable j: int = 0
        while j < n_features do
            let mutable column: float array = Array.empty<float>
            let mutable i: int = 0
            while i < n_samples do
                column <- Array.append column [|(_idx (_idx data (i)) (j))|]
                i <- i + 1
            let m: float = mean (column)
            means <- Array.append means [|m|]
            let mutable variance: float = 0.0
            let mutable k: int = 0
            while k < n_samples do
                let diff: float = (_idx column (k)) - m
                variance <- variance + (diff * diff)
                k <- k + 1
            stds <- Array.append stds [|(sqrt (variance / (float (n_samples - 1))))|]
            j <- j + 1
        let mutable standardized: float array array = Array.empty<float array>
        let mutable r: int = 0
        while r < n_samples do
            let mutable row: float array = Array.empty<float>
            let mutable c: int = 0
            while c < n_features do
                row <- Array.append row [|(((_idx (_idx data (r)) (c)) - (_idx means (c))) / (_idx stds (c)))|]
                c <- c + 1
            standardized <- Array.append standardized [|row|]
            r <- r + 1
        __ret <- standardized
        raise Return
        __ret
    with
        | Return -> __ret
let rec covariance_matrix (data: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable data = data
    try
        let n_samples: int = Seq.length (data)
        let n_features: int = Seq.length (_idx data (0))
        let mutable cov: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < n_features do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < n_features do
                let mutable sum: float = 0.0
                let mutable k: int = 0
                while k < n_samples do
                    sum <- sum + ((_idx (_idx data (k)) (i)) * (_idx (_idx data (k)) (j)))
                    k <- k + 1
                row <- Array.append row [|(sum / (float (n_samples - 1)))|]
                j <- j + 1
            cov <- Array.append cov [|row|]
            i <- i + 1
        __ret <- cov
        raise Return
        __ret
    with
        | Return -> __ret
let rec normalize (vec: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable vec = vec
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (vec)) do
            sum <- sum + ((_idx vec (i)) * (_idx vec (i)))
            i <- i + 1
        let n: float = sqrt (sum)
        let mutable res: float array = Array.empty<float>
        let mutable j: int = 0
        while j < (Seq.length (vec)) do
            res <- Array.append res [|((_idx vec (j)) / n)|]
            j <- j + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec eigen_decomposition_2x2 (matrix: float array array) =
    let mutable __ret : Eigen = Unchecked.defaultof<Eigen>
    let mutable matrix = matrix
    try
        let a: float = _idx (_idx matrix (0)) (0)
        let b: float = _idx (_idx matrix (0)) (1)
        let mutable c: float = _idx (_idx matrix (1)) (1)
        let diff: float = a - c
        let discriminant: float = sqrt ((diff * diff) + ((4.0 * b) * b))
        let lambda1: float = ((a + c) + discriminant) / 2.0
        let lambda2: float = ((a + c) - discriminant) / 2.0
        let mutable v1: float array = Array.empty<float>
        let mutable v2: float array = Array.empty<float>
        if b <> 0.0 then
            v1 <- normalize (unbox<float array> [|lambda1 - c; b|])
            v2 <- normalize (unbox<float array> [|lambda2 - c; b|])
        else
            v1 <- unbox<float array> [|1.0; 0.0|]
            v2 <- unbox<float array> [|0.0; 1.0|]
        let mutable eigenvalues: float array = unbox<float array> [|lambda1; lambda2|]
        let mutable eigenvectors: float array array = [|v1; v2|]
        if (_idx eigenvalues (0)) < (_idx eigenvalues (1)) then
            let tmp_val: float = _idx eigenvalues (0)
            eigenvalues.[0] <- _idx eigenvalues (1)
            eigenvalues.[1] <- tmp_val
            let tmp_vec: float array = _idx eigenvectors (0)
            eigenvectors.[0] <- _idx eigenvectors (1)
            eigenvectors.[1] <- tmp_vec
        __ret <- { _values = eigenvalues; _vectors = eigenvectors }
        raise Return
        __ret
    with
        | Return -> __ret
let rec transpose (matrix: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable matrix = matrix
    try
        let rows: int = Seq.length (matrix)
        let cols: int = Seq.length (_idx matrix (0))
        let mutable trans: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < cols do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < rows do
                row <- Array.append row [|(_idx (_idx matrix (j)) (i))|]
                j <- j + 1
            trans <- Array.append trans [|row|]
            i <- i + 1
        __ret <- trans
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_multiply (a: float array array) (b: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable b = b
    try
        let rows_a: int = Seq.length (a)
        let cols_a: int = Seq.length (_idx a (0))
        let rows_b: int = Seq.length (b)
        let cols_b: int = Seq.length (_idx b (0))
        if cols_a <> rows_b then
            failwith ("Incompatible matrices")
        let mutable result: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < rows_a do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < cols_b do
                let mutable sum: float = 0.0
                let mutable k: int = 0
                while k < cols_a do
                    sum <- sum + ((_idx (_idx a (i)) (k)) * (_idx (_idx b (k)) (j)))
                    k <- k + 1
                row <- Array.append row [|sum|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec apply_pca (data: float array array) (n_components: int) =
    let mutable __ret : PCAResult = Unchecked.defaultof<PCAResult>
    let mutable data = data
    let mutable n_components = n_components
    try
        let mutable standardized: float array array = standardize (data)
        let mutable cov: float array array = covariance_matrix (standardized)
        let eig: Eigen = eigen_decomposition_2x2 (cov)
        let eigenvalues: float array = eig._values
        let eigenvectors: float array array = eig._vectors
        let components: float array array = transpose (eigenvectors)
        let _transformed: float array array = matrix_multiply (standardized) (components)
        let total: float = (_idx eigenvalues (0)) + (_idx eigenvalues (1))
        let mutable ratios: float array = Array.empty<float>
        let mutable i: int = 0
        while i < n_components do
            ratios <- Array.append ratios [|((_idx eigenvalues (i)) / total)|]
            i <- i + 1
        __ret <- { _transformed = _transformed; _variance_ratio = ratios }
        raise Return
        __ret
    with
        | Return -> __ret
let data: float array array = [|[|2.5; 2.4|]; [|0.5; 0.7|]; [|2.2; 2.9|]; [|1.9; 2.2|]; [|3.1; 3.0|]; [|2.3; 2.7|]; [|2.0; 1.6|]; [|1.0; 1.1|]; [|1.5; 1.6|]; [|1.1; 0.9|]|]
let mutable result: PCAResult = apply_pca (data) (2)
printfn "%s" ("Transformed Data (first 5 rows):")
let mutable idx: int = 0
while idx < 5 do
    printfn "%s" (_repr (_idx (result._transformed) (idx)))
    idx <- idx + 1
printfn "%s" ("Explained Variance Ratio:")
printfn "%s" (_repr (result._variance_ratio))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

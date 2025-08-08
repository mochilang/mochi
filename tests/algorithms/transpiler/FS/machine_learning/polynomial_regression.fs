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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec design_matrix (xs: float array) (degree: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable xs = xs
    let mutable degree = degree
    try
        let mutable i: int = 0
        let mutable matrix: float array array = Array.empty<float array>
        while i < (Seq.length (xs)) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            let mutable pow: float = 1.0
            while j <= degree do
                row <- Array.append row [|pow|]
                pow <- pow * (_idx xs (i))
                j <- j + 1
            matrix <- Array.append matrix [|row|]
            i <- i + 1
        __ret <- matrix
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
        let mutable j: int = 0
        let mutable result: float array array = Array.empty<float array>
        while j < cols do
            let mutable row: float array = Array.empty<float>
            let mutable i: int = 0
            while i < rows do
                row <- Array.append row [|(_idx (_idx matrix (i)) (j))|]
                i <- i + 1
            result <- Array.append result [|row|]
            j <- j + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec matmul (A: float array array) (B: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable A = A
    let mutable B = B
    try
        let n: int = Seq.length (A)
        let m: int = Seq.length (_idx A (0))
        let p: int = Seq.length (_idx B (0))
        let mutable i: int = 0
        let mutable result: float array array = Array.empty<float array>
        while i < n do
            let mutable row: float array = Array.empty<float>
            let mutable k: int = 0
            while k < p do
                let mutable sum: float = 0.0
                let mutable j: int = 0
                while j < m do
                    sum <- sum + ((_idx (_idx A (i)) (j)) * (_idx (_idx B (j)) (k)))
                    j <- j + 1
                row <- Array.append row [|sum|]
                k <- k + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec matvec_mul (A: float array array) (v: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable A = A
    let mutable v = v
    try
        let n: int = Seq.length (A)
        let m: int = Seq.length (_idx A (0))
        let mutable i: int = 0
        let mutable result: float array = Array.empty<float>
        while i < n do
            let mutable sum: float = 0.0
            let mutable j: int = 0
            while j < m do
                sum <- sum + ((_idx (_idx A (i)) (j)) * (_idx v (j)))
                j <- j + 1
            result <- Array.append result [|sum|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec gaussian_elimination (A: float array array) (b: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable A = A
    let mutable b = b
    try
        let n: int = Seq.length (A)
        let mutable M: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < n do
            M <- Array.append M [|(Array.append (_idx A (i)) [|(_idx b (i))|])|]
            i <- i + 1
        let mutable k: int = 0
        while k < n do
            let mutable j: int = k + 1
            while j < n do
                let factor: float = (_idx (_idx M (j)) (k)) / (_idx (_idx M (k)) (k))
                let mutable rowj: float array = _idx M (j)
                let mutable rowk: float array = _idx M (k)
                let mutable l: int = k
                while l <= n do
                    rowj.[l] <- (_idx rowj (l)) - (factor * (_idx rowk (l)))
                    l <- l + 1
                M.[j] <- rowj
                j <- j + 1
            k <- k + 1
        let mutable x: float array = Array.empty<float>
        let mutable t: int = 0
        while t < n do
            x <- Array.append x [|0.0|]
            t <- t + 1
        let mutable i2: int = n - 1
        while i2 >= 0 do
            let mutable sum: float = _idx (_idx M (i2)) (n)
            let mutable j2: int = i2 + 1
            while j2 < n do
                sum <- sum - ((_idx (_idx M (i2)) (j2)) * (_idx x (j2)))
                j2 <- j2 + 1
            x.[i2] <- sum / (_idx (_idx M (i2)) (i2))
            i2 <- i2 - 1
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
let rec predict (xs: float array) (coeffs: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    let mutable coeffs = coeffs
    try
        let mutable i: int = 0
        let mutable result: float array = Array.empty<float>
        while i < (Seq.length (xs)) do
            let mutable x: float = _idx xs (i)
            let mutable j: int = 0
            let mutable pow: float = 1.0
            let mutable sum: float = 0.0
            while j < (Seq.length (coeffs)) do
                sum <- sum + ((_idx coeffs (j)) * pow)
                pow <- pow * x
                j <- j + 1
            result <- Array.append result [|sum|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let xs: float array = unbox<float array> [|0.0; 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0|]
let mutable ys: float array = Array.empty<float>
let mutable i: int = 0
while i < (Seq.length (xs)) do
    let mutable x: float = _idx xs (i)
    ys <- Array.append ys [|(((((x * x) * x) - ((2.0 * x) * x)) + (3.0 * x)) - 5.0)|]
    i <- i + 1
let mutable X: float array array = design_matrix (xs) (3)
let mutable Xt: float array array = transpose (X)
let mutable XtX: float array array = matmul (Xt) (X)
let mutable Xty: float array = matvec_mul (Xt) (ys)
let coeffs: float array = gaussian_elimination (XtX) (Xty)
printfn "%s" (_str (coeffs))
printfn "%s" (_str (predict (unbox<float array> [|-1.0|]) (coeffs)))
printfn "%s" (_str (predict (unbox<float array> [|-2.0|]) (coeffs)))
printfn "%s" (_str (predict (unbox<float array> [|6.0|]) (coeffs)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

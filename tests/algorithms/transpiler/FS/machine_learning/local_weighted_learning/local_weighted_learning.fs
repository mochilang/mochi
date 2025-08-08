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
let json (arr:obj) =
    match arr with
    | :? (int array array) as a2 ->
        printf "[\n"
        for i in 0 .. a2.Length - 1 do
            let line = String.concat ", " (Array.map string a2.[i] |> Array.toList)
            if i < a2.Length - 1 then
                printfn "  [%s]," line
            else
                printfn "  [%s]" line
        printfn "]"
    | :? (int array) as a1 ->
        let line = String.concat ", " (Array.map string a1 |> Array.toList)
        printfn "[%s]" line
    | _ -> ()
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec expApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x < 0.0 then
            __ret <- 1.0 / (expApprox (-x))
            raise Return
        if x > 1.0 then
            let half: float = expApprox (x / 2.0)
            __ret <- half * half
            raise Return
        let mutable sum: float = 1.0
        let mutable term: float = 1.0
        let mutable n: int = 1
        while n < 20 do
            term <- (term * x) / (float n)
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec transpose (mat: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable mat = mat
    try
        let rows: int = Seq.length (mat)
        let cols: int = Seq.length (_idx mat (0))
        let mutable res: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < cols do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < rows do
                row <- Array.append row [|(_idx (_idx mat (j)) (i))|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec matMul (a: float array array) (b: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable b = b
    try
        let a_rows: int = Seq.length (a)
        let a_cols: int = Seq.length (_idx a (0))
        let b_cols: int = Seq.length (_idx b (0))
        let mutable res: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < a_rows do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < b_cols do
                let mutable sum: float = 0.0
                let mutable k: int = 0
                while k < a_cols do
                    sum <- sum + ((_idx (_idx a (i)) (k)) * (_idx (_idx b (k)) (j)))
                    k <- k + 1
                row <- Array.append row [|sum|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec matInv (mat: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable mat = mat
    try
        let mutable n: int = Seq.length (mat)
        let mutable aug: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < n do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|(_idx (_idx mat (i)) (j))|]
                j <- j + 1
            j <- 0
            while j < n do
                if i = j then
                    row <- Array.append row [|1.0|]
                else
                    row <- Array.append row [|0.0|]
                j <- j + 1
            aug <- Array.append aug [|row|]
            i <- i + 1
        let mutable col: int = 0
        while col < n do
            let pivot: float = _idx (_idx aug (col)) (col)
            if pivot = 0.0 then
                failwith ("Matrix is singular")
            let mutable j: int = 0
            while j < (2 * n) do
                aug.[col].[j] <- (_idx (_idx aug (col)) (j)) / pivot
                j <- j + 1
            let mutable r: int = 0
            while r < n do
                if r <> col then
                    let factor: float = _idx (_idx aug (r)) (col)
                    j <- 0
                    while j < (2 * n) do
                        aug.[r].[j] <- (_idx (_idx aug (r)) (j)) - (factor * (_idx (_idx aug (col)) (j)))
                        j <- j + 1
                r <- r + 1
            col <- col + 1
        let mutable inv: float array array = Array.empty<float array>
        i <- 0
        while i < n do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|(_idx (_idx aug (i)) (j + n))|]
                j <- j + 1
            inv <- Array.append inv [|row|]
            i <- i + 1
        __ret <- inv
        raise Return
        __ret
    with
        | Return -> __ret
let rec weight_matrix (point: float array) (x_train: float array array) (tau: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable point = point
    let mutable x_train = x_train
    let mutable tau = tau
    try
        let m: int = Seq.length (x_train)
        let mutable weights: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < m do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < m do
                if i = j then
                    row <- Array.append row [|1.0|]
                else
                    row <- Array.append row [|0.0|]
                j <- j + 1
            weights <- Array.append weights [|row|]
            i <- i + 1
        let mutable j: int = 0
        while j < m do
            let mutable diff_sq: float = 0.0
            let mutable k: int = 0
            while k < (Seq.length (point)) do
                let diff: float = (_idx point (k)) - (_idx (_idx x_train (j)) (k))
                diff_sq <- diff_sq + (diff * diff)
                k <- k + 1
            weights.[j].[j] <- expApprox ((-diff_sq) / ((2.0 * tau) * tau))
            j <- j + 1
        __ret <- weights
        raise Return
        __ret
    with
        | Return -> __ret
let rec local_weight (point: float array) (x_train: float array array) (y_train: float array) (tau: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable point = point
    let mutable x_train = x_train
    let mutable y_train = y_train
    let mutable tau = tau
    try
        let w: float array array = weight_matrix (point) (x_train) (tau)
        let x_t: float array array = transpose (x_train)
        let x_t_w: float array array = matMul (x_t) (w)
        let x_t_w_x: float array array = matMul (x_t_w) (x_train)
        let inv_part: float array array = matInv (x_t_w_x)
        let mutable y_col: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (Seq.length (y_train)) do
            y_col <- Array.append y_col [|[|_idx y_train (i)|]|]
            i <- i + 1
        let x_t_w_y: float array array = matMul (x_t_w) (y_col)
        __ret <- matMul (inv_part) (x_t_w_y)
        raise Return
        __ret
    with
        | Return -> __ret
let rec local_weight_regression (x_train: float array array) (y_train: float array) (tau: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable x_train = x_train
    let mutable y_train = y_train
    let mutable tau = tau
    try
        let m: int = Seq.length (x_train)
        let mutable preds: float array = Array.empty<float>
        let mutable i: int = 0
        while i < m do
            let theta: float array array = local_weight (_idx x_train (i)) (x_train) (y_train) (tau)
            let mutable weights_vec: float array = Array.empty<float>
            let mutable k: int = 0
            while k < (Seq.length (theta)) do
                weights_vec <- Array.append weights_vec [|(_idx (_idx theta (k)) (0))|]
                k <- k + 1
            let mutable pred: float = 0.0
            let mutable j: int = 0
            while j < (Seq.length (_idx x_train (i))) do
                pred <- pred + ((_idx (_idx x_train (i)) (j)) * (_idx weights_vec (j)))
                j <- j + 1
            preds <- Array.append preds [|pred|]
            i <- i + 1
        __ret <- preds
        raise Return
        __ret
    with
        | Return -> __ret
let x_train: float array array = [|[|16.99; 10.34|]; [|21.01; 23.68|]; [|24.59; 25.69|]|]
let y_train: float array = unbox<float array> [|1.01; 1.66; 3.5|]
let mutable preds: float array = local_weight_regression (x_train) (y_train) (0.6)
json (preds)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

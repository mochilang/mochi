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
type LanczosResult = {
    mutable _t: float array array
    mutable _q: float array array
}
type EigenResult = {
    mutable _values: float array
    mutable _vectors: float array array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let mutable _seed: int = 123456789
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        _seed <- int ((((int64 ((_seed * 1103515245) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- _seed
        raise Return
        __ret
    with
        | Return -> __ret
let rec random () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        __ret <- (1.0 * (float (rand()))) / 2147483648.0
        raise Return
        __ret
    with
        | Return -> __ret
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
let rec absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec dot (a: float array) (b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let mutable s: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            s <- s + ((_idx a (i)) * (_idx b (i)))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec vector_scale (v: float array) (s: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable v = v
    let mutable s = s
    try
        let mutable res: float array = [||]
        let mutable i: int = 0
        while i < (Seq.length (v)) do
            res <- Array.append res [|((_idx v (i)) * s)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec vector_sub (a: float array) (b: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: float array = [||]
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            res <- Array.append res [|((_idx a (i)) - (_idx b (i)))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec vector_add (a: float array) (b: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: float array = [||]
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            res <- Array.append res [|((_idx a (i)) + (_idx b (i)))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec zeros_matrix (r: int) (c: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable r = r
    let mutable c = c
    try
        let mutable m: float array array = [||]
        let mutable i: int = 0
        while i < r do
            let mutable row: float array = [||]
            let mutable j: int = 0
            while j < c do
                row <- Array.append row [|0.0|]
                j <- j + 1
            m <- Array.append m [|row|]
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
let rec column (m: float array array) (idx: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable m = m
    let mutable idx = idx
    try
        let mutable col: float array = [||]
        let mutable i: int = 0
        while i < (Seq.length (m)) do
            col <- Array.append col [|(_idx (_idx m (i)) (idx))|]
            i <- i + 1
        __ret <- col
        raise Return
        __ret
    with
        | Return -> __ret
let rec validate_adjacency_list (graph: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable graph = graph
    try
        let mutable i: int = 0
        while i < (Seq.length (graph)) do
            let mutable j: int = 0
            while j < (Seq.length (_idx graph (i))) do
                let mutable v: int = _idx (_idx graph (i)) (j)
                if (v < 0) || (v >= (Seq.length (graph))) then
                    failwith ("Invalid neighbor")
                j <- j + 1
            i <- i + 1
        __ret
    with
        | Return -> __ret
let rec multiply_matrix_vector (graph: int array array) (vector: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable graph = graph
    let mutable vector = vector
    try
        let n: int = Seq.length (graph)
        if (Seq.length (vector)) <> n then
            failwith ("Vector length must match number of nodes")
        let mutable result: float array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable sum: float = 0.0
            let mutable j: int = 0
            while j < (Seq.length (_idx graph (i))) do
                let nb: int = _idx (_idx graph (i)) (j)
                sum <- sum + (_idx vector (nb))
                j <- j + 1
            result <- Array.append result [|sum|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec lanczos_iteration (graph: int array array) (k: int) =
    let mutable __ret : LanczosResult = Unchecked.defaultof<LanczosResult>
    let mutable graph = graph
    let mutable k = k
    try
        let n: int = Seq.length (graph)
        if (k < 1) || (k > n) then
            failwith ("invalid number of eigenvectors")
        let mutable _q: float array array = zeros_matrix (n) (k)
        let mutable _t: float array array = zeros_matrix (k) (k)
        let mutable v: float array = [||]
        let mutable i: int = 0
        while i < n do
            v <- Array.append v [|(random())|]
            i <- i + 1
        let mutable ss: float = 0.0
        i <- 0
        while i < n do
            ss <- ss + ((_idx v (i)) * (_idx v (i)))
            i <- i + 1
        let vnorm: float = sqrtApprox (ss)
        i <- 0
        while i < n do
            _q.[i].[0] <- (_idx v (i)) / vnorm
            i <- i + 1
        let mutable beta: float = 0.0
        let mutable j: int = 0
        while j < k do
            let mutable w: float array = multiply_matrix_vector (graph) (column (_q) (j))
            if j > 0 then
                w <- vector_sub (w) (vector_scale (column (_q) (j - 1)) (beta))
            let alpha: float = dot (column (_q) (j)) (w)
            w <- vector_sub (w) (vector_scale (column (_q) (j)) (alpha))
            let mutable ss2: float = 0.0
            let mutable p: int = 0
            while p < n do
                ss2 <- ss2 + ((_idx w (p)) * (_idx w (p)))
                p <- p + 1
            beta <- sqrtApprox (ss2)
            _t.[j].[j] <- alpha
            if j < (k - 1) then
                _t.[j].[j + 1] <- beta
                _t.[j + 1].[j] <- beta
                if beta > 0.0000000001 then
                    let mutable wnorm: float array = vector_scale (w) (1.0 / beta)
                    let mutable r: int = 0
                    while r < n do
                        _q.[r].[j + 1] <- _idx wnorm (r)
                        r <- r + 1
            j <- j + 1
        __ret <- { _t = _t; _q = _q }
        raise Return
        __ret
    with
        | Return -> __ret
let rec jacobi_eigen (a_in: float array array) (max_iter: int) =
    let mutable __ret : EigenResult = Unchecked.defaultof<EigenResult>
    let mutable a_in = a_in
    let mutable max_iter = max_iter
    try
        let n: int = Seq.length (a_in)
        let mutable a: float array array = a_in
        let mutable v: float array array = zeros_matrix (n) (n)
        let mutable i: int = 0
        while i < n do
            v.[i].[i] <- 1.0
            i <- i + 1
        let mutable iter: int = 0
        try
            while iter < max_iter do
                try
                    let mutable p: int = 0
                    let mutable _q: int = 1
                    let mutable max: float = absf (_idx (_idx a (p)) (_q))
                    i <- 0
                    while i < n do
                        let mutable j: int = i + 1
                        while j < n do
                            let ``val``: float = absf (_idx (_idx a (i)) (j))
                            if ``val`` > max then
                                max <- ``val``
                                p <- i
                                _q <- j
                            j <- j + 1
                        i <- i + 1
                    if max < 0.00000001 then
                        raise Break
                    let app: float = _idx (_idx a (p)) (p)
                    let aqq: float = _idx (_idx a (_q)) (_q)
                    let apq: float = _idx (_idx a (p)) (_q)
                    let theta: float = (aqq - app) / (2.0 * apq)
                    let mutable _t: float = 1.0 / ((absf (theta)) + (sqrtApprox ((theta * theta) + 1.0)))
                    if theta < 0.0 then
                        _t <- -_t
                    let c: float = 1.0 / (sqrtApprox (1.0 + (_t * _t)))
                    let mutable s: float = _t * c
                    let tau: float = s / (1.0 + c)
                    a.[p].[p] <- app - (_t * apq)
                    a.[_q].[_q] <- aqq + (_t * apq)
                    a.[p].[_q] <- 0.0
                    a.[_q].[p] <- 0.0
                    let mutable k: int = 0
                    while k < n do
                        if (k <> p) && (k <> _q) then
                            let akp: float = _idx (_idx a (k)) (p)
                            let akq: float = _idx (_idx a (k)) (_q)
                            a.[k].[p] <- akp - (s * (akq + (tau * akp)))
                            a.[p].[k] <- _idx (_idx a (k)) (p)
                            a.[k].[_q] <- akq + (s * (akp - (tau * akq)))
                            a.[_q].[k] <- _idx (_idx a (k)) (_q)
                        k <- k + 1
                    k <- 0
                    while k < n do
                        let vkp: float = _idx (_idx v (k)) (p)
                        let vkq: float = _idx (_idx v (k)) (_q)
                        v.[k].[p] <- vkp - (s * (vkq + (tau * vkp)))
                        v.[k].[_q] <- vkq + (s * (vkp - (tau * vkq)))
                        k <- k + 1
                    iter <- iter + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable eigenvalues: float array = [||]
        i <- 0
        while i < n do
            eigenvalues <- Array.append eigenvalues [|(_idx (_idx a (i)) (i))|]
            i <- i + 1
        __ret <- { _values = eigenvalues; _vectors = v }
        raise Return
        __ret
    with
        | Return -> __ret
let rec matmul (a: float array array) (b: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable b = b
    try
        let rows: int = Seq.length (a)
        let cols: int = Seq.length (_idx b (0))
        let inner: int = Seq.length (b)
        let mutable m: float array array = zeros_matrix (rows) (cols)
        let mutable i: int = 0
        while i < rows do
            let mutable j: int = 0
            while j < cols do
                let mutable s: float = 0.0
                let mutable k: int = 0
                while k < inner do
                    s <- s + ((_idx (_idx a (i)) (k)) * (_idx (_idx b (k)) (j)))
                    k <- k + 1
                m.[i].[j] <- s
                j <- j + 1
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
let rec sort_eigenpairs (vals: float array) (vecs: float array array) =
    let mutable __ret : EigenResult = Unchecked.defaultof<EigenResult>
    let mutable vals = vals
    let mutable vecs = vecs
    try
        let n: int = Seq.length (vals)
        let mutable _values: float array = vals
        let mutable _vectors: float array array = vecs
        let mutable i: int = 0
        while i < n do
            let mutable j: int = 0
            while j < (n - 1) do
                if (_idx _values (j)) < (_idx _values (j + 1)) then
                    let tmp: float = _idx _values (j)
                    _values.[j] <- _idx _values (j + 1)
                    _values.[j + 1] <- tmp
                    let mutable r: int = 0
                    while r < (Seq.length (_vectors)) do
                        let tv: float = _idx (_idx _vectors (r)) (j)
                        _vectors.[r].[j] <- _idx (_idx _vectors (r)) (j + 1)
                        _vectors.[r].[j + 1] <- tv
                        r <- r + 1
                j <- j + 1
            i <- i + 1
        __ret <- { _values = _values; _vectors = _vectors }
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_lanczos_eigenvectors (graph: int array array) (k: int) =
    let mutable __ret : EigenResult = Unchecked.defaultof<EigenResult>
    let mutable graph = graph
    let mutable k = k
    try
        validate_adjacency_list (graph)
        let mutable res: LanczosResult = lanczos_iteration (graph) (k)
        let eig: EigenResult = jacobi_eigen (res._t) (50)
        let sorted: EigenResult = sort_eigenpairs (eig._values) (eig._vectors)
        let final_vectors: float array array = matmul (res._q) (sorted._vectors)
        __ret <- { _values = sorted._values; _vectors = final_vectors }
        raise Return
        __ret
    with
        | Return -> __ret
let rec list_to_string (arr: float array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable arr = arr
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            s <- s + (_str (_idx arr (i)))
            if i < ((Seq.length (arr)) - 1) then
                s <- s + ", "
            i <- i + 1
        __ret <- s + "]"
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_to_string (m: float array array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable m = m
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (m)) do
            s <- s + (list_to_string (_idx m (i)))
            if i < ((Seq.length (m)) - 1) then
                s <- s + "; "
            i <- i + 1
        __ret <- s + "]"
        raise Return
        __ret
    with
        | Return -> __ret
let graph: int array array = [|[|1; 2|]; [|0; 2|]; [|0; 1|]|]
let mutable result: EigenResult = find_lanczos_eigenvectors (graph) (2)
printfn "%s" (list_to_string (result._values))
printfn "%s" (matrix_to_string (result._vectors))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

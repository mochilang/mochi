// Generated 2025-08-08 16:34 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec zeros (n: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable n = n
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < n do
            res <- Array.append res [|0.0|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec dot (a: float array) (b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            sum <- sum + ((_idx a (i)) * (_idx b (i)))
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec mat_vec_mul (m: float array array) (v: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable m = m
    let mutable v = v
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (m)) do
            let mutable s: float = 0.0
            let mutable j: int = 0
            while j < (Seq.length (_idx m (i))) do
                s <- s + ((_idx (_idx m (i)) (j)) * (_idx v (j)))
                j <- j + 1
            res <- Array.append res [|s|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec vec_add (a: float array) (b: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            res <- Array.append res [|((_idx a (i)) + (_idx b (i)))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec vec_sub (a: float array) (b: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            res <- Array.append res [|((_idx a (i)) - (_idx b (i)))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec scalar_mul (s: float) (v: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable s = s
    let mutable v = v
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (v)) do
            res <- Array.append res [|(s * (_idx v (i)))|]
            i <- i + 1
        __ret <- res
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
let rec norm (v: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable v = v
    try
        __ret <- sqrtApprox (dot (v) (v))
        raise Return
        __ret
    with
        | Return -> __ret
let rec conjugate_gradient (A: float array array) (b: float array) (max_iterations: int) (tol: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable A = A
    let mutable b = b
    let mutable max_iterations = max_iterations
    let mutable tol = tol
    try
        let n: int = Seq.length (b)
        let mutable x: float array = zeros (n)
        let mutable r: float array = vec_sub (b) (mat_vec_mul (A) (x))
        let mutable p: float array = r
        let mutable rs_old: float = dot (r) (r)
        let mutable i: int = 0
        try
            while i < max_iterations do
                try
                    let Ap: float array = mat_vec_mul (A) (p)
                    let alpha: float = rs_old / (dot (p) (Ap))
                    x <- vec_add (x) (scalar_mul (alpha) (p))
                    r <- vec_sub (r) (scalar_mul (alpha) (Ap))
                    let rs_new: float = dot (r) (r)
                    if (sqrtApprox (rs_new)) < tol then
                        raise Break
                    let beta: float = rs_new / rs_old
                    p <- vec_add (r) (scalar_mul (beta) (p))
                    rs_old <- rs_new
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
let A: float array array = [|[|8.73256573; -5.02034289; -2.68709226|]; [|-5.02034289; 3.78188322; 0.91980451|]; [|-2.68709226; 0.91980451; 1.94746467|]|]
let b: float array = unbox<float array> [|-5.80872761; 3.23807431; 1.95381422|]
let mutable x: float array = conjugate_gradient (A) (b) (1000) (0.00000001)
printfn "%s" (_str (_idx x (0)))
printfn "%s" (_str (_idx x (1)))
printfn "%s" (_str (_idx x (2)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

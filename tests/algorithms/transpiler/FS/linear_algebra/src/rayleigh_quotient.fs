// Generated 2025-08-08 16:34 +0700

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
type Complex = {
    mutable _re: float
    mutable _im: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec complex_conj (z: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable z = z
    try
        __ret <- { _re = z._re; _im = -(z._im) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec complex_eq (a: Complex) (b: Complex) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        __ret <- ((a._re) = (b._re)) && ((a._im) = (b._im))
        raise Return
        __ret
    with
        | Return -> __ret
let rec complex_add (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { _re = (a._re) + (b._re); _im = (a._im) + (b._im) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec complex_mul (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        let real: float = ((a._re) * (b._re)) - ((a._im) * (b._im))
        let imag: float = ((a._re) * (b._im)) + ((a._im) * (b._re))
        __ret <- { _re = real; _im = imag }
        raise Return
        __ret
    with
        | Return -> __ret
let rec conj_vector (v: Complex array) =
    let mutable __ret : Complex array = Unchecked.defaultof<Complex array>
    let mutable v = v
    try
        let mutable res: Complex array = Array.empty<Complex>
        let mutable i: int = 0
        while i < (Seq.length (v)) do
            res <- Array.append res [|(complex_conj (_idx v (i)))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec vec_mat_mul (v: Complex array) (m: Complex array array) =
    let mutable __ret : Complex array = Unchecked.defaultof<Complex array>
    let mutable v = v
    let mutable m = m
    try
        let mutable result: Complex array = Array.empty<Complex>
        let mutable col: int = 0
        while col < (Seq.length (_idx m (0))) do
            let mutable sum: Complex = { _re = 0.0; _im = 0.0 }
            let mutable row: int = 0
            while row < (Seq.length (v)) do
                sum <- complex_add (sum) (complex_mul (_idx v (row)) (_idx (_idx m (row)) (col)))
                row <- row + 1
            result <- Array.append result [|sum|]
            col <- col + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec dot (a: Complex array) (b: Complex array) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        let mutable sum: Complex = { _re = 0.0; _im = 0.0 }
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            sum <- complex_add (sum) (complex_mul (_idx a (i)) (_idx b (i)))
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_hermitian (m: Complex array array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable m = m
    try
        let mutable i: int = 0
        while i < (Seq.length (m)) do
            let mutable j: int = 0
            while j < (Seq.length (m)) do
                if not (complex_eq (_idx (_idx m (i)) (j)) (complex_conj (_idx (_idx m (j)) (i)))) then
                    __ret <- false
                    raise Return
                j <- j + 1
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec rayleigh_quotient (a: Complex array array) (v: Complex array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable v = v
    try
        let v_star: Complex array = conj_vector (v)
        let v_star_dot: Complex array = vec_mat_mul (v_star) (a)
        let num: Complex = dot (v_star_dot) (v)
        let den: Complex = dot (v_star) (v)
        __ret <- (num._re) / (den._re)
        raise Return
        __ret
    with
        | Return -> __ret
let a: Complex array array = [|[|{ _re = 2.0; _im = 0.0 }; { _re = 2.0; _im = 1.0 }; { _re = 4.0; _im = 0.0 }|]; [|{ _re = 2.0; _im = -1.0 }; { _re = 3.0; _im = 0.0 }; { _re = 0.0; _im = 1.0 }|]; [|{ _re = 4.0; _im = 0.0 }; { _re = 0.0; _im = -1.0 }; { _re = 1.0; _im = 0.0 }|]|]
let v: Complex array = unbox<Complex array> [|{ _re = 1.0; _im = 0.0 }; { _re = 2.0; _im = 0.0 }; { _re = 3.0; _im = 0.0 }|]
if is_hermitian (a) then
    let r1: float = rayleigh_quotient (a) (v)
    printfn "%g" (r1)
    printfn "%s" ("\n")
let b: Complex array array = [|[|{ _re = 1.0; _im = 0.0 }; { _re = 2.0; _im = 0.0 }; { _re = 4.0; _im = 0.0 }|]; [|{ _re = 2.0; _im = 0.0 }; { _re = 3.0; _im = 0.0 }; { _re = -1.0; _im = 0.0 }|]; [|{ _re = 4.0; _im = 0.0 }; { _re = -1.0; _im = 0.0 }; { _re = 1.0; _im = 0.0 }|]|]
if is_hermitian (b) then
    let r2: float = rayleigh_quotient (b) (v)
    printfn "%g" (r2)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

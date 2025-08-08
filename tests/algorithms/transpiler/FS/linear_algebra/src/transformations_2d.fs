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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let PI: float = 3.141592653589793
let rec floor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable i: int = int x
        if (float i) > x then
            i <- i - 1
        __ret <- float i
        raise Return
        __ret
    with
        | Return -> __ret
let rec modf (x: float) (m: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable m = m
    try
        __ret <- x - ((floor (x / m)) * m)
        raise Return
        __ret
    with
        | Return -> __ret
let rec sin_taylor (angle: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable angle = angle
    try
        let mutable x: float = modf (angle) (2.0 * PI)
        if x > PI then
            x <- x - (2.0 * PI)
        let mutable term: float = x
        let mutable sum: float = x
        let mutable i: int = 1
        while i < 10 do
            let k1: float = 2.0 * (float i)
            let k2: float = k1 + 1.0
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec cos_taylor (angle: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable angle = angle
    try
        let mutable x: float = modf (angle) (2.0 * PI)
        if x > PI then
            x <- x - (2.0 * PI)
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: int = 1
        while i < 10 do
            let k1: float = (2.0 * (float i)) - 1.0
            let k2: float = 2.0 * (float i)
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
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
            let mutable row: float array = _idx m (i)
            s <- s + "["
            let mutable j: int = 0
            while j < (Seq.length (row)) do
                s <- s + (_str (_idx row (j)))
                if j < ((Seq.length (row)) - 1) then
                    s <- s + ", "
                j <- j + 1
            s <- s + "]"
            if i < ((Seq.length (m)) - 1) then
                s <- s + ", "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec scaling (f: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable f = f
    try
        __ret <- [|[|f; 0.0|]; [|0.0; f|]|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec rotation (angle: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable angle = angle
    try
        let c: float = cos_taylor (angle)
        let mutable s: float = sin_taylor (angle)
        __ret <- [|[|c; -s|]; [|s; c|]|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec projection (angle: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable angle = angle
    try
        let c: float = cos_taylor (angle)
        let mutable s: float = sin_taylor (angle)
        let cs: float = c * s
        __ret <- [|[|c * c; cs|]; [|cs; s * s|]|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec reflection (angle: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable angle = angle
    try
        let c: float = cos_taylor (angle)
        let mutable s: float = sin_taylor (angle)
        let cs: float = c * s
        __ret <- [|[|(2.0 * c) - 1.0; 2.0 * cs|]; [|2.0 * cs; (2.0 * s) - 1.0|]|]
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" ("    scaling(5) = " + (matrix_to_string (scaling (5.0))))
printfn "%s" ("  rotation(45) = " + (matrix_to_string (rotation (45.0))))
printfn "%s" ("projection(45) = " + (matrix_to_string (projection (45.0))))
printfn "%s" ("reflection(45) = " + (matrix_to_string (reflection (45.0))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

// Generated 2025-08-09 10:14 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec check_matrix (mat: float array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mat = mat
    try
        if ((Seq.length (mat)) < 2) || ((Seq.length (_idx mat (int 0))) < 2) then
            failwith ("Expected a matrix with at least 2x2 dimensions")
        __ret
    with
        | Return -> __ret
and add (a: float array array) (b: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable b = b
    try
        check_matrix (a)
        check_matrix (b)
        if ((Seq.length (a)) <> (Seq.length (b))) || ((Seq.length (_idx a (int 0))) <> (Seq.length (_idx b (int 0)))) then
            failwith ("Matrices must have the same dimensions")
        let rows: int = Seq.length (a)
        let cols: int = Seq.length (_idx a (int 0))
        let mutable result: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < rows do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < cols do
                row <- Array.append row [|((_idx (_idx a (int i)) (int j)) + (_idx (_idx b (int i)) (int j)))|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and subtract (a: float array array) (b: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable b = b
    try
        check_matrix (a)
        check_matrix (b)
        if ((Seq.length (a)) <> (Seq.length (b))) || ((Seq.length (_idx a (int 0))) <> (Seq.length (_idx b (int 0)))) then
            failwith ("Matrices must have the same dimensions")
        let rows: int = Seq.length (a)
        let cols: int = Seq.length (_idx a (int 0))
        let mutable result: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < rows do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < cols do
                row <- Array.append row [|((_idx (_idx a (int i)) (int j)) - (_idx (_idx b (int i)) (int j)))|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and scalar_multiply (a: float array array) (s: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable s = s
    try
        check_matrix (a)
        let rows: int = Seq.length (a)
        let cols: int = Seq.length (_idx a (int 0))
        let mutable result: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < rows do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < cols do
                row <- Array.append row [|((_idx (_idx a (int i)) (int j)) * s)|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and multiply (a: float array array) (b: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable b = b
    try
        check_matrix (a)
        check_matrix (b)
        if (Seq.length (_idx a (int 0))) <> (Seq.length (b)) then
            failwith ("Invalid dimensions for matrix multiplication")
        let rows: int = Seq.length (a)
        let cols: int = Seq.length (_idx b (int 0))
        let mutable result: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < rows do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < cols do
                let mutable sum: float = 0.0
                let mutable k: int = 0
                while k < (Seq.length (b)) do
                    sum <- sum + ((_idx (_idx a (int i)) (int k)) * (_idx (_idx b (int k)) (int j)))
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
and identity (n: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable n = n
    try
        let mutable result: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < n do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < n do
                if i = j then
                    row <- Array.append row [|1.0|]
                else
                    row <- Array.append row [|0.0|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and transpose (a: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    try
        check_matrix (a)
        let rows: int = Seq.length (a)
        let cols: int = Seq.length (_idx a (int 0))
        let mutable result: float array array = Array.empty<float array>
        let mutable j: int = 0
        while j < cols do
            let mutable row: float array = Array.empty<float>
            let mutable i: int = 0
            while i < rows do
                row <- Array.append row [|(_idx (_idx a (int i)) (int j))|]
                i <- i + 1
            result <- Array.append result [|row|]
            j <- j + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mat_a: float array array = [|[|12.0; 10.0|]; [|3.0; 9.0|]|]
        let mat_b: float array array = [|[|3.0; 4.0|]; [|7.0; 4.0|]|]
        let mat_c: float array array = [|[|3.0; 0.0; 2.0|]; [|2.0; 0.0; -2.0|]; [|0.0; 1.0; 1.0|]|]
        printfn "%s" (_str (add (mat_a) (mat_b)))
        printfn "%s" (_str (subtract (mat_a) (mat_b)))
        printfn "%s" (_str (multiply (mat_a) (mat_b)))
        printfn "%s" (_str (scalar_multiply (mat_a) (3.5)))
        printfn "%s" (_str (identity (5)))
        printfn "%s" (_str (transpose (mat_c)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

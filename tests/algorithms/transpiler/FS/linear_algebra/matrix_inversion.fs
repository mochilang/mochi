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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec invert_matrix (matrix: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable matrix = matrix
    try
        let n: int = Seq.length (matrix)
        let mutable aug: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < n do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|(_idx (_idx matrix (i)) (j))|]
                j <- j + 1
            let mutable k: int = 0
            while k < n do
                if i = k then
                    row <- Array.append row [|1.0|]
                else
                    row <- Array.append row [|0.0|]
                k <- k + 1
            aug <- Array.append aug [|row|]
            i <- i + 1
        let mutable col: int = 0
        try
            while col < n do
                try
                    let mutable pivot_row: int = col
                    let mutable r: int = col
                    try
                        while r < n do
                            try
                                if (_idx (_idx aug (r)) (col)) <> 0.0 then
                                    pivot_row <- r
                                    raise Break
                                r <- r + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if (_idx (_idx aug (pivot_row)) (col)) = 0.0 then
                        failwith ("Matrix is not invertible")
                    if pivot_row <> col then
                        let temp: float array = _idx aug (col)
                        aug.[col] <- _idx aug (pivot_row)
                        aug.[pivot_row] <- temp
                    let pivot: float = _idx (_idx aug (col)) (col)
                    let mutable c: int = 0
                    while c < (2 * n) do
                        aug.[col].[c] <- (_idx (_idx aug (col)) (c)) / pivot
                        c <- c + 1
                    let mutable r2: int = 0
                    while r2 < n do
                        if r2 <> col then
                            let factor: float = _idx (_idx aug (r2)) (col)
                            let mutable c2: int = 0
                            while c2 < (2 * n) do
                                aug.[r2].[c2] <- (_idx (_idx aug (r2)) (c2)) - (factor * (_idx (_idx aug (col)) (c2)))
                                c2 <- c2 + 1
                        r2 <- r2 + 1
                    col <- col + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable inv: float array array = Array.empty<float array>
        let mutable r3: int = 0
        while r3 < n do
            let mutable row: float array = Array.empty<float>
            let mutable c3: int = 0
            while c3 < n do
                row <- Array.append row [|(_idx (_idx aug (r3)) (c3 + n))|]
                c3 <- c3 + 1
            inv <- Array.append inv [|row|]
            r3 <- r3 + 1
        __ret <- inv
        raise Return
        __ret
    with
        | Return -> __ret
let mutable mat: float array array = [|[|4.0; 7.0|]; [|2.0; 6.0|]|]
printfn "%s" ("Original Matrix:")
printfn "%s" (_repr (mat))
printfn "%s" ("Inverted Matrix:")
printfn "%s" (_repr (invert_matrix (mat)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

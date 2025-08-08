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
type LU = {
    mutable _lower: float array array
    mutable _upper: float array array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec lu_decomposition (mat: float array array) =
    let mutable __ret : LU = Unchecked.defaultof<LU>
    let mutable mat = mat
    try
        let n: int = Seq.length (mat)
        if n = 0 then
            __ret <- { _lower = Array.empty<float array>; _upper = Array.empty<float array> }
            raise Return
        let m: int = Seq.length (_idx mat (0))
        if n <> m then
            failwith ("Matrix must be square")
        let mutable _lower: float array array = Array.empty<float array>
        let mutable _upper: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < n do
            let mutable lrow: float array = Array.empty<float>
            let mutable urow: float array = Array.empty<float>
            let mutable j: int = 0
            while j < n do
                lrow <- Array.append lrow [|0.0|]
                urow <- Array.append urow [|0.0|]
                j <- j + 1
            _lower <- Array.append _lower [|lrow|]
            _upper <- Array.append _upper [|urow|]
            i <- i + 1
        i <- 0
        while i < n do
            let mutable j1: int = 0
            while j1 < i do
                let mutable total: float = 0.0
                let mutable k: int = 0
                while k < i do
                    total <- total + ((_idx (_idx _lower (i)) (k)) * (_idx (_idx _upper (k)) (j1)))
                    k <- k + 1
                if (_idx (_idx _upper (j1)) (j1)) = 0.0 then
                    failwith ("No LU decomposition exists")
                _lower.[i].[j1] <- ((_idx (_idx mat (i)) (j1)) - total) / (_idx (_idx _upper (j1)) (j1))
                j1 <- j1 + 1
            _lower.[i].[i] <- 1.0
            let mutable j2: int = i
            while j2 < n do
                let mutable total2: float = 0.0
                let mutable k2: int = 0
                while k2 < i do
                    total2 <- total2 + ((_idx (_idx _lower (i)) (k2)) * (_idx (_idx _upper (k2)) (j2)))
                    k2 <- k2 + 1
                _upper.[i].[j2] <- (_idx (_idx mat (i)) (j2)) - total2
                j2 <- j2 + 1
            i <- i + 1
        __ret <- { _lower = _lower; _upper = _upper }
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_matrix (mat: float array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mat = mat
    try
        let mutable i: int = 0
        while i < (Seq.length (mat)) do
            let mutable line: string = ""
            let mutable j: int = 0
            while j < (Seq.length (_idx mat (i))) do
                line <- line + (_str (_idx (_idx mat (i)) (j)))
                if (j + 1) < (Seq.length (_idx mat (i))) then
                    line <- line + " "
                j <- j + 1
            printfn "%s" (line)
            i <- i + 1
        __ret
    with
        | Return -> __ret
let matrix: float array array = [|[|2.0; -2.0; 1.0|]; [|0.0; 1.0; 2.0|]; [|5.0; 3.0; 1.0|]|]
let result: LU = lu_decomposition (matrix)
print_matrix (result._lower)
print_matrix (result._upper)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

// Generated 2025-08-08 17:07 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec pivot (t: float array array) (row: int) (col: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable t = t
    let mutable row = row
    let mutable col = col
    try
        let mutable pivotRow: float array = Array.empty<float>
        let pivotVal: float = _idx (_idx t (row)) (col)
        for j in 0 .. ((Seq.length (_idx t (row))) - 1) do
            pivotRow <- Array.append pivotRow [|((_idx (_idx t (row)) (j)) / pivotVal)|]
        t.[row] <- pivotRow
        for i in 0 .. ((Seq.length (t)) - 1) do
            if i <> row then
                let factor: float = _idx (_idx t (i)) (col)
                let mutable newRow: float array = Array.empty<float>
                for j in 0 .. ((Seq.length (_idx t (i))) - 1) do
                    let value: float = (_idx (_idx t (i)) (j)) - (factor * (_idx pivotRow (j)))
                    newRow <- Array.append newRow [|value|]
                t.[i] <- newRow
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
let rec findPivot (t: float array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable t = t
    try
        let mutable col: int = 0
        let mutable minVal: float = 0.0
        for j in 0 .. (((Seq.length (_idx t (0))) - 1) - 1) do
            let v: float = _idx (_idx t (0)) (j)
            if v < minVal then
                minVal <- v
                col <- j
        if minVal >= 0.0 then
            __ret <- unbox<int array> [|-1; -1|]
            raise Return
        let mutable row: int = -1
        let mutable minRatio: float = 0.0
        let mutable first: bool = true
        for i in 1 .. ((Seq.length (t)) - 1) do
            let coeff: float = _idx (_idx t (i)) (col)
            if coeff > 0.0 then
                let rhs: float = _idx (_idx t (i)) ((Seq.length (_idx t (i))) - 1)
                let ratio: float = rhs / coeff
                if first || (ratio < minRatio) then
                    minRatio <- ratio
                    row <- i
                    first <- false
        __ret <- unbox<int array> [|row; col|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec interpret (t: float array array) (nVars: int) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, float> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, float>>
    let mutable t = t
    let mutable nVars = nVars
    try
        let lastCol: int = (Seq.length (_idx t (0))) - 1
        let mutable p: float = _idx (_idx t (0)) (lastCol)
        if p < 0.0 then
            p <- -p
        let mutable result: System.Collections.Generic.IDictionary<string, float> = _dictCreate []
        result.["P"] <- p
        for i in 0 .. (nVars - 1) do
            let mutable nzRow: int = -1
            let mutable nzCount: int = 0
            for r in 0 .. ((Seq.length (t)) - 1) do
                let ``val``: float = _idx (_idx t (r)) (i)
                if ``val`` <> 0.0 then
                    nzCount <- nzCount + 1
                    nzRow <- r
            if (nzCount = 1) && ((_idx (_idx t (nzRow)) (i)) = 1.0) then
                result.["x" + (_str (i + 1))] <- _idx (_idx t (nzRow)) (lastCol)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec simplex (tab: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable tab = tab
    try
        let mutable t: float array array = tab
        try
            while true do
                try
                    let mutable p: int array = findPivot (t)
                    let mutable row: int = _idx p (0)
                    let mutable col: int = _idx p (1)
                    if row < 0 then
                        raise Break
                    t <- pivot (t) (row) (col)
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
let mutable tableau: float array array = [|[|-1.0; -1.0; 0.0; 0.0; 0.0|]; [|1.0; 3.0; 1.0; 0.0; 4.0|]; [|3.0; 1.0; 0.0; 1.0; 4.0|]|]
let mutable finalTab: float array array = simplex (tableau)
let res: System.Collections.Generic.IDictionary<string, float> = interpret (finalTab) (2)
printfn "%s" ("P: " + (_str (_dictGet res ((string ("P"))))))
for i in 0 .. (2 - 1) do
    let key: string = "x" + (_str (i + 1))
    if res.ContainsKey(key) then
        printfn "%s" ((key + ": ") + (_str (_dictGet res ((string (key))))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

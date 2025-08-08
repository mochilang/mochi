// Generated 2025-08-08 18:58 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
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
and pow10 (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable p: float = 1.0
        let mutable i: int = 0
        while i < n do
            p <- p * 10.0
            i <- i + 1
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
and round (x: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable n = n
    try
        let m: float = pow10 (n)
        __ret <- (floor ((x * m) + 0.5)) / m
        raise Return
        __ret
    with
        | Return -> __ret
and clone_matrix (mat: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable mat = mat
    try
        let mutable new_mat: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (Seq.length (mat)) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (Seq.length (_idx mat (int i))) do
                row <- Array.append row [|(_idx (_idx mat (int i)) (int j))|]
                j <- j + 1
            new_mat <- Array.append new_mat [|row|]
            i <- i + 1
        __ret <- new_mat
        raise Return
        __ret
    with
        | Return -> __ret
and solve_simultaneous (equations: float array array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable equations = equations
    try
        let n: int = Seq.length (equations)
        if n = 0 then
            failwith ("solve_simultaneous() requires n lists of length n+1")
        let m: int = n + 1
        let mutable i: int = 0
        while i < n do
            if (Seq.length (_idx equations (int i))) <> m then
                failwith ("solve_simultaneous() requires n lists of length n+1")
            i <- i + 1
        let mutable a: float array array = clone_matrix (equations)
        let mutable row: int = 0
        while row < n do
            let mutable pivot: int = row
            while (pivot < n) && ((_idx (_idx a (int pivot)) (int row)) = 0.0) do
                pivot <- pivot + 1
            if pivot = n then
                failwith ("solve_simultaneous() requires at least 1 full equation")
            if pivot <> row then
                let temp: float array = _idx a (int row)
                a.[int row] <- _idx a (int pivot)
                a.[int pivot] <- temp
            let pivot_val: float = _idx (_idx a (int row)) (int row)
            let mutable col: int = 0
            while col < m do
                a.[int row].[int col] <- (_idx (_idx a (int row)) (int col)) / pivot_val
                col <- col + 1
            let mutable r: int = 0
            while r < n do
                if r <> row then
                    let factor: float = _idx (_idx a (int r)) (int row)
                    let mutable c: int = 0
                    while c < m do
                        a.[int r].[int c] <- (_idx (_idx a (int r)) (int c)) - (factor * (_idx (_idx a (int row)) (int c)))
                        c <- c + 1
                r <- r + 1
            row <- row + 1
        let mutable res: float array = Array.empty<float>
        let mutable k: int = 0
        while k < n do
            res <- Array.append res [|(round (_idx (_idx a (int k)) (int (m - 1))) (5))|]
            k <- k + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and test_solver () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let a: float array array = [|[|1.0; 2.0; 3.0|]; [|4.0; 5.0; 6.0|]|]
        let r1: float array = solve_simultaneous (a)
        if not ((((Seq.length (r1)) = 2) && ((_idx r1 (int 0)) = (0.0 - 1.0))) && ((_idx r1 (int 1)) = 2.0)) then
            failwith ("test1 failed")
        let b: float array array = [|[|0.0; 0.0 - 3.0; 1.0; 7.0|]; [|3.0; 2.0; 0.0 - 1.0; 11.0|]; [|5.0; 1.0; 0.0 - 2.0; 12.0|]|]
        let r2: float array = solve_simultaneous (b)
        if not (((((Seq.length (r2)) = 3) && ((_idx r2 (int 0)) = 6.4)) && ((_idx r2 (int 1)) = 1.2)) && ((_idx r2 (int 2)) = 10.6)) then
            failwith ("test2 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_solver()
        let eq: float array array = [|[|2.0; 1.0; 1.0; 1.0; 1.0; 4.0|]; [|1.0; 2.0; 1.0; 1.0; 1.0; 5.0|]; [|1.0; 1.0; 2.0; 1.0; 1.0; 6.0|]; [|1.0; 1.0; 1.0; 2.0; 1.0; 7.0|]; [|1.0; 1.0; 1.0; 1.0; 2.0; 8.0|]|]
        printfn "%s" (_str (solve_simultaneous (eq)))
        printfn "%s" (_str (solve_simultaneous ([|[|4.0; 2.0|]|])))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

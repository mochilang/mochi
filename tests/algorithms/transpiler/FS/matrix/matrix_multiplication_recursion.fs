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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec is_square (matrix: int array array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable matrix = matrix
    try
        let n: int = Seq.length (matrix)
        let mutable i: int = 0
        while i < n do
            if (Seq.length (_idx matrix (int i))) <> n then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_multiply (a: int array array) (b: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable a = a
    let mutable b = b
    try
        let rows: int = Seq.length (a)
        let cols: int = Seq.length (_idx b (int 0))
        let inner: int = Seq.length (b)
        let mutable result: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < rows do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < cols do
                let mutable sum: int = 0
                let mutable k: int = 0
                while k < inner do
                    sum <- int ((int64 sum) + ((int64 (_idx (_idx a (int i)) (int k))) * (int64 (_idx (_idx b (int k)) (int j)))))
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
let rec multiply (i: int) (j: int) (k: int) (a: int array array) (b: int array array) (result: int array array) (n: int) (m: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable i = i
    let mutable j = j
    let mutable k = k
    let mutable a = a
    let mutable b = b
    let mutable result = result
    let mutable n = n
    let mutable m = m
    try
        if i >= n then
            __ret <- ()
            raise Return
        if j >= m then
            multiply (i + 1) (0) (0) (a) (b) (result) (n) (m)
            __ret <- ()
            raise Return
        if k >= (Seq.length (b)) then
            multiply (i) (j + 1) (0) (a) (b) (result) (n) (m)
            __ret <- ()
            raise Return
        result.[int i].[int j] <- int ((int64 (_idx (_idx result (int i)) (int j))) + ((int64 (_idx (_idx a (int i)) (int k))) * (int64 (_idx (_idx b (int k)) (int j)))))
        multiply (i) (j) (k + 1) (a) (b) (result) (n) (m)
        __ret
    with
        | Return -> __ret
let rec matrix_multiply_recursive (a: int array array) (b: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable a = a
    let mutable b = b
    try
        if ((Seq.length (a)) = 0) || ((Seq.length (b)) = 0) then
            __ret <- Array.empty<int array>
            raise Return
        if (((Seq.length (a)) <> (Seq.length (b))) || (not (is_square (a)))) || (not (is_square (b))) then
            failwith ("Invalid matrix dimensions")
        let n: int = Seq.length (a)
        let m: int = Seq.length (_idx b (int 0))
        let mutable result: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < n do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < m do
                row <- Array.append row [|0|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        multiply (0) (0) (0) (a) (b) (result) (n) (m)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let matrix_1_to_4: int array array = [|[|1; 2|]; [|3; 4|]|]
let matrix_5_to_8: int array array = [|[|5; 6|]; [|7; 8|]|]
let matrix_count_up: int array array = [|[|1; 2; 3; 4|]; [|5; 6; 7; 8|]; [|9; 10; 11; 12|]; [|13; 14; 15; 16|]|]
let matrix_unordered: int array array = [|[|5; 8; 1; 2|]; [|6; 7; 3; 0|]; [|4; 5; 9; 1|]; [|2; 6; 10; 14|]|]
printfn "%s" (_repr (matrix_multiply_recursive (matrix_1_to_4) (matrix_5_to_8)))
printfn "%s" (_repr (matrix_multiply_recursive (matrix_count_up) (matrix_unordered)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

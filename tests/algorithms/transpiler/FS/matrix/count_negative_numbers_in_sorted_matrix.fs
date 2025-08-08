// Generated 2025-08-08 18:58 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec generate_large_matrix () =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    try
        let mutable result: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < 1000 do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 1000 - i
            while j > ((-1000) - i) do
                row <- Array.append row [|j|]
                j <- j - 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_negative_index (arr: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    try
        let mutable left: int = 0
        let mutable right: int = (Seq.length (arr)) - 1
        if (Seq.length (arr)) = 0 then
            __ret <- 0
            raise Return
        if (_idx arr (int 0)) < 0 then
            __ret <- 0
            raise Return
        while left <= right do
            let mid: int = _floordiv (left + right) 2
            let num: int = _idx arr (int mid)
            if num < 0 then
                if mid = 0 then
                    __ret <- 0
                    raise Return
                if (_idx arr (int (mid - 1))) >= 0 then
                    __ret <- mid
                    raise Return
                right <- mid - 1
            else
                left <- mid + 1
        __ret <- Seq.length (arr)
        raise Return
        __ret
    with
        | Return -> __ret
let rec count_negatives_binary_search (grid: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable grid = grid
    try
        let mutable total: int = 0
        let mutable bound: int = Seq.length (_idx grid (int 0))
        let mutable i: int = 0
        while i < (Seq.length (grid)) do
            let mutable row: int array = _idx grid (int i)
            let idx: int = find_negative_index (Array.sub row 0 (bound - 0))
            bound <- idx
            total <- total + idx
            i <- i + 1
        __ret <- int (((int64 (Seq.length (grid))) * (int64 (Seq.length (_idx grid (int 0))))) - (int64 total))
        raise Return
        __ret
    with
        | Return -> __ret
let rec count_negatives_brute_force (grid: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable grid = grid
    try
        let mutable count: int = 0
        let mutable i: int = 0
        while i < (Seq.length (grid)) do
            let mutable row: int array = _idx grid (int i)
            let mutable j: int = 0
            while j < (Seq.length (row)) do
                if (_idx row (int j)) < 0 then
                    count <- count + 1
                j <- j + 1
            i <- i + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
let rec count_negatives_brute_force_with_break (grid: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable grid = grid
    try
        let mutable total: int = 0
        let mutable i: int = 0
        try
            while i < (Seq.length (grid)) do
                try
                    let mutable row: int array = _idx grid (int i)
                    let mutable j: int = 0
                    try
                        while j < (Seq.length (row)) do
                            try
                                let number: int = _idx row (int j)
                                if number < 0 then
                                    total <- total + ((Seq.length (row)) - j)
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
let grid: int array array = generate_large_matrix()
let test_grids: int array array array = [|[|[|4; 3; 2; -1|]; [|3; 2; 1; -1|]; [|1; 1; -1; -2|]; [|-1; -1; -2; -3|]|]; [|[|3; 2|]; [|1; 0|]|]; [|[|7; 7; 6|]|]; [|[|7; 7; 6|]; [|-1; -2; -3|]|]; grid|]
let mutable results_bin: int array = Array.empty<int>
let mutable i: int = 0
while i < (Seq.length (test_grids)) do
    results_bin <- Array.append results_bin [|(count_negatives_binary_search (_idx test_grids (int i)))|]
    i <- i + 1
printfn "%s" (_str (results_bin))
let mutable results_brute: int array = Array.empty<int>
i <- 0
while i < (Seq.length (test_grids)) do
    results_brute <- Array.append results_brute [|(count_negatives_brute_force (_idx test_grids (int i)))|]
    i <- i + 1
printfn "%s" (_str (results_brute))
let mutable results_break: int array = Array.empty<int>
i <- 0
while i < (Seq.length (test_grids)) do
    results_break <- Array.append results_break [|(count_negatives_brute_force_with_break (_idx test_grids (int i)))|]
    i <- i + 1
printfn "%s" (_str (results_break))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

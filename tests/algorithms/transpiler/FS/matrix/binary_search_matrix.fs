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
let rec binary_search (arr: int array) (lower_bound: int) (upper_bound: int) (value: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable lower_bound = lower_bound
    let mutable upper_bound = upper_bound
    let mutable value = value
    try
        let r: int = _floordiv (lower_bound + upper_bound) 2
        if (_idx arr (int r)) = value then
            __ret <- r
            raise Return
        if lower_bound >= upper_bound then
            __ret <- -1
            raise Return
        if (_idx arr (int r)) < value then
            __ret <- binary_search (arr) (r + 1) (upper_bound) (value)
            raise Return
        __ret <- binary_search (arr) (lower_bound) (r - 1) (value)
        raise Return
        __ret
    with
        | Return -> __ret
and mat_bin_search (value: int) (matrix: int array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable value = value
    let mutable matrix = matrix
    try
        let mutable index: int = 0
        if (_idx (_idx matrix (int index)) (int 0)) = value then
            __ret <- unbox<int array> [|index; 0|]
            raise Return
        while (index < (Seq.length (matrix))) && ((_idx (_idx matrix (int index)) (int 0)) < value) do
            let r: int = binary_search (_idx matrix (int index)) (0) ((Seq.length (_idx matrix (int index))) - 1) (value)
            if r <> (-1) then
                __ret <- unbox<int array> [|index; r|]
                raise Return
            index <- index + 1
        __ret <- unbox<int array> [|-1; -1|]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let row: int array = unbox<int array> [|1; 4; 7; 11; 15|]
        printfn "%s" (_str (binary_search (row) (0) ((Seq.length (row)) - 1) (1)))
        printfn "%s" (_str (binary_search (row) (0) ((Seq.length (row)) - 1) (23)))
        let matrix: int array array = [|[|1; 4; 7; 11; 15|]; [|2; 5; 8; 12; 19|]; [|3; 6; 9; 16; 22|]; [|10; 13; 14; 17; 24|]; [|18; 21; 23; 26; 30|]|]
        printfn "%s" (_str (mat_bin_search (1) (matrix)))
        printfn "%s" (_str (mat_bin_search (34) (matrix)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

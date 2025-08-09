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
let rec search_in_sorted_matrix (mat: float array array) (m: int) (n: int) (key: float) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mat = mat
    let mutable m = m
    let mutable n = n
    let mutable key = key
    try
        let mutable i: int = m - 1
        let mutable j: int = 0
        while (i >= 0) && (j < n) do
            if key = (_idx (_idx mat (int i)) (int j)) then
                printfn "%s" ((((("Key " + (_str (key))) + " found at row- ") + (_str (i + 1))) + " column- ") + (_str (j + 1)))
                __ret <- ()
                raise Return
            if key < (_idx (_idx mat (int i)) (int j)) then
                i <- i - 1
            else
                j <- j + 1
        printfn "%s" (("Key " + (_str (key))) + " not found")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mat: float array array = [|[|2.0; 5.0; 7.0|]; [|4.0; 8.0; 13.0|]; [|9.0; 11.0; 15.0|]; [|12.0; 17.0; 20.0|]|]
        search_in_sorted_matrix (mat) (Seq.length (mat)) (Seq.length (_idx mat (int 0))) (5.0)
        search_in_sorted_matrix (mat) (Seq.length (mat)) (Seq.length (_idx mat (int 0))) (21.0)
        let mat2: float array array = [|[|2.1; 5.0; 7.0|]; [|4.0; 8.0; 13.0|]; [|9.0; 11.0; 15.0|]; [|12.0; 17.0; 20.0|]|]
        search_in_sorted_matrix (mat2) (Seq.length (mat2)) (Seq.length (_idx mat2 (int 0))) (2.1)
        search_in_sorted_matrix (mat2) (Seq.length (mat2)) (Seq.length (_idx mat2 (int 0))) (2.2)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

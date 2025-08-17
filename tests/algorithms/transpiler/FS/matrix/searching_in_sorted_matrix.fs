// Generated 2025-08-17 13:19 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let rec search_in_sorted_matrix (mat: float array array) (m: int) (n: int) (key: float) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable mat = mat
    let mutable m = m
    let mutable n = n
    let mutable key = key
    try
        let mutable i: int = m - 1
        let mutable j: int = 0
        while (i >= 0) && (j < n) do
            if key = (_idx (_idx mat (int i)) (int j)) then
                ignore (printfn "%s" ((((("Key " + (_str (key))) + " found at row- ") + (_str (i + 1))) + " column- ") + (_str (j + 1))))
                __ret <- ()
                raise Return
            if key < (_idx (_idx mat (int i)) (int j)) then
                i <- i - 1
            else
                j <- j + 1
        ignore (printfn "%s" (("Key " + (_str (key))) + " not found"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mat: float array array = [|[|2.0; 5.0; 7.0|]; [|4.0; 8.0; 13.0|]; [|9.0; 11.0; 15.0|]; [|12.0; 17.0; 20.0|]|]
        ignore (search_in_sorted_matrix (mat) (Seq.length (mat)) (Seq.length (_idx mat (int 0))) (5.0))
        ignore (search_in_sorted_matrix (mat) (Seq.length (mat)) (Seq.length (_idx mat (int 0))) (21.0))
        let mat2: float array array = [|[|2.1; 5.0; 7.0|]; [|4.0; 8.0; 13.0|]; [|9.0; 11.0; 15.0|]; [|12.0; 17.0; 20.0|]|]
        ignore (search_in_sorted_matrix (mat2) (Seq.length (mat2)) (Seq.length (_idx mat2 (int 0))) (2.1))
        ignore (search_in_sorted_matrix (mat2) (Seq.length (mat2)) (Seq.length (_idx mat2 (int 0))) (2.2))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())

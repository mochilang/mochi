// Generated 2025-08-07 15:46 +0700

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
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec peak (lst: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable lst = lst
    try
        let mutable low: int = 0
        let mutable high: int = (Seq.length (lst)) - 1
        while low < high do
            let mid: int = (low + high) / 2
            if (_idx lst (mid)) < (_idx lst (mid + 1)) then
                low <- mid + 1
            else
                high <- mid
        __ret <- _idx lst (low)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (_str (peak (unbox<int array> [|1; 2; 3; 4; 5; 4; 3; 2; 1|])))
        printfn "%s" (_str (peak (unbox<int array> [|1; 10; 9; 8; 7; 6; 5; 4|])))
        printfn "%s" (_str (peak (unbox<int array> [|1; 9; 8; 7|])))
        printfn "%s" (_str (peak (unbox<int array> [|1; 2; 3; 4; 5; 6; 7; 0|])))
        printfn "%s" (_str (peak (unbox<int array> [|1; 2; 3; 4; 3; 2; 1; 0; -1; -2|])))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

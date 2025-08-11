// Generated 2025-08-11 16:20 +0700

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
let rec search (list_data: int array) (key: int) (left: int) (right: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable list_data = list_data
    let mutable key = key
    let mutable left = left
    let mutable right = right
    try
        let mutable r: int = right
        if r = 0 then
            r <- (Seq.length (list_data)) - 1
        if left > r then
            __ret <- -1
            raise Return
        else
            if (_idx list_data (int left)) = key then
                __ret <- left
                raise Return
            else
                if (_idx list_data (int r)) = key then
                    __ret <- r
                    raise Return
                else
                    __ret <- search (list_data) (key) (left + 1) (r - 1)
                    raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%d" (search (unbox<int array> [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]) (5) (0) (0))
        printfn "%d" (search (unbox<int array> [|1; 2; 4; 5; 3|]) (4) (0) (0))
        printfn "%d" (search (unbox<int array> [|1; 2; 4; 5; 3|]) (6) (0) (0))
        printfn "%d" (search (unbox<int array> [|5|]) (5) (0) (0))
        printfn "%d" (search (Array.empty<int>) (1) (0) (0))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

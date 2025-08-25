// Generated 2025-08-25 22:27 +0700

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
    | :? float as f ->
        if f = floor f then sprintf "%g.0" f else sprintf "%g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("L", "")
         .Replace("\"", "")
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let rec perfect (n: int64) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n <= (int64 0) then
            __ret <- false
            raise Return
        let limit: int64 = _floordiv64 (int64 n) (int64 (int64 2))
        let mutable sum: int64 = int64 0
        let mutable i: int64 = int64 1
        while i <= limit do
            if (((n % i + i) % i)) = (int64 0) then
                sum <- sum + i
            i <- i + (int64 1)
        __ret <- sum = n
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let numbers: int array = unbox<int array> [|6; 28; 29; 12; 496; 8128; 0; -1|]
        let mutable idx: int64 = int64 0
        while idx < (int64 (Seq.length (numbers))) do
            let num: int64 = int64 (_idx numbers (int idx))
            if perfect (int64 num) then
                ignore (printfn "%s" ((_str (num)) + " is a Perfect Number."))
            else
                ignore (printfn "%s" ((_str (num)) + " is not a Perfect Number."))
            idx <- idx + (int64 1)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())

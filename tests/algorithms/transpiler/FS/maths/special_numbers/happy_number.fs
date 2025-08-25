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
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let rec is_happy_number (num: int64) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable num = num
    try
        if num <= (int64 0) then
            ignore (failwith ("num must be a positive integer"))
        let mutable seen: int64 array = Array.empty<int64>
        let mutable n: int64 = num
        while n <> (int64 1) do
            let mutable i: int64 = int64 0
            while i < (int64 (Seq.length (seen))) do
                if (_idx seen (int i)) = n then
                    __ret <- false
                    raise Return
                i <- i + (int64 1)
            seen <- Array.append seen [|n|]
            let mutable total: int64 = int64 0
            let mutable temp: int64 = n
            while temp > (int64 0) do
                let digit: int64 = ((temp % (int64 10) + (int64 10)) % (int64 10))
                total <- total + (digit * digit)
                temp <- _floordiv64 (int64 temp) (int64 (int64 10))
            n <- total
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and test_is_happy_number () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if not (is_happy_number (int64 19)) then
            ignore (failwith ("19 should be happy"))
        if is_happy_number (int64 2) then
            ignore (failwith ("2 should be unhappy"))
        if not (is_happy_number (int64 23)) then
            ignore (failwith ("23 should be happy"))
        if not (is_happy_number (int64 1)) then
            ignore (failwith ("1 should be happy"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (test_is_happy_number())
        ignore (printfn "%b" (is_happy_number (int64 19)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())

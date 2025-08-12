// Generated 2025-08-12 13:41 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec pow_int (``base``: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and digital_sum (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let s: string = _str (n)
        let mutable sum: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            sum <- sum + (int (string (s.[i])))
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and solution (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable max_sum: int = 0
        let mutable ``base``: int = 0
        while ``base`` < a do
            let mutable power: int = 0
            while power < b do
                let value: int = pow_int (``base``) (power)
                let ds: int = digital_sum (value)
                if ds > max_sum then
                    max_sum <- ds
                power <- power + 1
            ``base`` <- ``base`` + 1
        __ret <- max_sum
        raise Return
        __ret
    with
        | Return -> __ret
and test_solution () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (solution (10) (10)) <> 45 then
            ignore (failwith ("solution 10 10 failed"))
        if (solution (100) (100)) <> 972 then
            ignore (failwith ("solution 100 100 failed"))
        if (solution (100) (200)) <> 1872 then
            ignore (failwith ("solution 100 200 failed"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_solution()
        ignore (printfn "%s" (_str (solution (100) (100))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

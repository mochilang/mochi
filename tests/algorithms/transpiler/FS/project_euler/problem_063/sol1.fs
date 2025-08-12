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
let rec pow (``base``: int) (exponent: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exponent = exponent
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < exponent do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and num_digits (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n = 0 then
            __ret <- 1
            raise Return
        let mutable count: int = 0
        let mutable x: int = n
        while x > 0 do
            x <- _floordiv x 10
            count <- count + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and solution (max_base: int) (max_power: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable max_base = max_base
    let mutable max_power = max_power
    try
        let mutable total: int = 0
        let mutable ``base``: int = 1
        while ``base`` < max_base do
            let mutable power: int = 1
            while power < max_power do
                let digits: int = num_digits (pow (``base``) (power))
                if digits = power then
                    total <- total + 1
                power <- power + 1
            ``base`` <- ``base`` + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" ("solution(10, 22) = " + (_str (solution (10) (22)))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

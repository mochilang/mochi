// Generated 2025-08-07 10:31 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Prefix = {
    name: string
    exp: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let si_positive: Prefix array = [|{ name = "yotta"; exp = 24 }; { name = "zetta"; exp = 21 }; { name = "exa"; exp = 18 }; { name = "peta"; exp = 15 }; { name = "tera"; exp = 12 }; { name = "giga"; exp = 9 }; { name = "mega"; exp = 6 }; { name = "kilo"; exp = 3 }; { name = "hecto"; exp = 2 }; { name = "deca"; exp = 1 }|]
let si_negative: Prefix array = [|{ name = "deci"; exp = -1 }; { name = "centi"; exp = -2 }; { name = "milli"; exp = -3 }; { name = "micro"; exp = -6 }; { name = "nano"; exp = -9 }; { name = "pico"; exp = -12 }; { name = "femto"; exp = -15 }; { name = "atto"; exp = -18 }; { name = "zepto"; exp = -21 }; { name = "yocto"; exp = -24 }|]
let binary_prefixes: Prefix array = [|{ name = "yotta"; exp = 80 }; { name = "zetta"; exp = 70 }; { name = "exa"; exp = 60 }; { name = "peta"; exp = 50 }; { name = "tera"; exp = 40 }; { name = "giga"; exp = 30 }; { name = "mega"; exp = 20 }; { name = "kilo"; exp = 10 }|]
let rec pow (``base``: float) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable e: int = exp
        if e < 0 then
            e <- -e
            let mutable i: int = 0
            while i < e do
                result <- result * ``base``
                i <- i + 1
            __ret <- 1.0 / result
            raise Return
        let mutable i: int = 0
        while i < e do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec add_si_prefix (value: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable value = value
    try
        let mutable prefixes: Prefix array = Array.empty<Prefix>
        if value > 0.0 then
            prefixes <- si_positive
        else
            prefixes <- si_negative
        let mutable i: int = 0
        while i < (Seq.length (prefixes)) do
            let p: Prefix = _idx prefixes (i)
            let num: float = value / (pow (10.0) (p.exp))
            if num > 1.0 then
                __ret <- ((_str (num)) + " ") + (p.name)
                raise Return
            i <- i + 1
        __ret <- _str (value)
        raise Return
        __ret
    with
        | Return -> __ret
let rec add_binary_prefix (value: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable value = value
    try
        let mutable i: int = 0
        while i < (Seq.length (binary_prefixes)) do
            let p: Prefix = _idx binary_prefixes (i)
            let num: float = value / (pow (2.0) (p.exp))
            if num > 1.0 then
                __ret <- ((_str (num)) + " ") + (p.name)
                raise Return
            i <- i + 1
        __ret <- _str (value)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (add_si_prefix (10000.0))
printfn "%s" (add_si_prefix (0.005))
printfn "%s" (add_binary_prefix (65536.0))
printfn "%s" (add_binary_prefix (512.0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

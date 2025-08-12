// Generated 2025-08-12 12:40 +0700

exception Break
exception Continue

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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
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
let rec int_sqrt (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable x: int = 1
        while ((x + 1) * (x + 1)) <= n do
            x <- x + 1
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and solution (limit: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable limit = limit
    try
        let mutable sum_divs: int array = Array.empty<int>
        let mutable i: int = 0
        while i <= limit do
            sum_divs <- Array.append sum_divs [|1|]
            i <- i + 1
        let sqrt_limit: int = int_sqrt (limit)
        i <- 2
        while i <= sqrt_limit do
            sum_divs.[(i * i)] <- (_idx sum_divs (int (i * i))) + i
            let mutable k: int = i + 1
            while k <= (_floordiv limit i) do
                sum_divs.[(k * i)] <- ((_idx sum_divs (int (k * i))) + k) + i
                k <- k + 1
            i <- i + 1
        let mutable is_abundant: bool array = Array.empty<bool>
        i <- 0
        while i <= limit do
            is_abundant <- Array.append is_abundant [|false|]
            i <- i + 1
        let mutable abundants: int array = Array.empty<int>
        let mutable res: int = 0
        let mutable n: int = 1
        try
            while n <= limit do
                try
                    if (_idx sum_divs (int n)) > n then
                        abundants <- Array.append abundants [|n|]
                        is_abundant.[n] <- true
                    let mutable has_pair: bool = false
                    let mutable j: int = 0
                    try
                        while j < (Seq.length (abundants)) do
                            try
                                let a: int = _idx abundants (int j)
                                if a > n then
                                    raise Break
                                let b: int = n - a
                                if (b <= limit) && (_idx is_abundant (int b)) then
                                    has_pair <- true
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if not has_pair then
                        res <- res + n
                    n <- n + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (solution (28123))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

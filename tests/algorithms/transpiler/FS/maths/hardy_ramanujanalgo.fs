// Generated 2025-08-12 07:47 +0700

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
let rec exact_prime_factor_count (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable count: int = 0
        let mutable num: int = n
        if (((num % 2 + 2) % 2)) = 0 then
            count <- count + 1
            while (((num % 2 + 2) % 2)) = 0 do
                num <- _floordiv num 2
        let mutable i: int = 3
        while ((int64 i) * (int64 i)) <= (int64 num) do
            if (((num % i + i) % i)) = 0 then
                count <- count + 1
                while (((num % i + i) % i)) = 0 do
                    num <- _floordiv num i
            i <- i + 2
        if num > 2 then
            count <- count + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and ln (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let ln2: float = 0.6931471805599453
        let mutable y: float = x
        let mutable k: float = 0.0
        while y > 2.0 do
            y <- y / 2.0
            k <- k + ln2
        while y < 1.0 do
            y <- y * 2.0
            k <- k - ln2
        let t: float = (y - 1.0) / (y + 1.0)
        let mutable term: float = t
        let mutable sum: float = 0.0
        let mutable n: int = 1
        while n <= 19 do
            sum <- sum + (term / (float n))
            term <- (term * t) * t
            n <- n + 2
        __ret <- k + (2.0 * sum)
        raise Return
        __ret
    with
        | Return -> __ret
and floor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable i: int = int x
        if (float i) > x then
            i <- i - 1
        __ret <- float i
        raise Return
        __ret
    with
        | Return -> __ret
and round4 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let m: float = 10000.0
        __ret <- (floor ((x * m) + 0.5)) / m
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable n: int = 51242183
        let mutable count: int = exact_prime_factor_count (n)
        printfn "%s" ("The number of distinct prime factors is/are " + (_str (count)))
        let loglog: float = ln (ln (float n))
        printfn "%s" ("The value of log(log(n)) is " + (_str (round4 (loglog))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

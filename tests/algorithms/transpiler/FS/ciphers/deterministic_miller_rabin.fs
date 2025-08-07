// Generated 2025-08-07 08:16 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec mod_pow (``base``: int) (exp: int) (``mod``: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    let mutable ``mod`` = ``mod``
    try
        let mutable result: int = 1
        let mutable b: int = ((``base`` % ``mod`` + ``mod``) % ``mod``)
        let mutable e: int = exp
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                result <- (((result * b) % ``mod`` + ``mod``) % ``mod``)
            b <- (((b * b) % ``mod`` + ``mod``) % ``mod``)
            e <- e / 2
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec miller_rabin (n: int) (allow_probable: bool) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    let mutable allow_probable = allow_probable
    try
        if n = 2 then
            __ret <- true
            raise Return
        if (n < 2) || ((((n % 2 + 2) % 2)) = 0) then
            __ret <- false
            raise Return
        if n > 5 then
            let last: int = ((n % 10 + 10) % 10)
            if not ((((last = 1) || (last = 3)) || (last = 7)) || (last = 9)) then
                __ret <- false
                raise Return
        let limit: int64 = 3825123056546413051L
        if ((int64 n) > limit) && (not allow_probable) then
            failwith ("Warning: upper bound of deterministic test is exceeded. Pass allow_probable=true to allow probabilistic test.")
        let bounds: int64 array = [|int64 (2047); int64 (1373653); int64 (25326001); 3215031751L; 2152302898747L; 3474749660383L; 341550071728321L; limit|]
        let primes: int array = [|2; 3; 5; 7; 11; 13; 17; 19|]
        let mutable i: int = 0
        let mutable plist_len: int = Seq.length (primes)
        while i < (Seq.length (bounds)) do
            if (int64 n) < (_idx bounds (i)) then
                plist_len <- i + 1
                i <- Seq.length (bounds)
            else
                i <- i + 1
        let mutable d: int = n - 1
        let mutable s: int = 0
        while (((d % 2 + 2) % 2)) = 0 do
            d <- d / 2
            s <- s + 1
        let mutable j: int = 0
        while j < plist_len do
            let prime: int = _idx primes (j)
            let mutable x: int = mod_pow (prime) (d) (n)
            let mutable pr: bool = false
            if (x = 1) || (x = (n - 1)) then
                pr <- true
            else
                let mutable r: int = 1
                while (r < s) && (not pr) do
                    x <- (((x * x) % n + n) % n)
                    if x = (n - 1) then
                        pr <- true
                    r <- r + 1
            if not pr then
                __ret <- false
                raise Return
            j <- j + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (miller_rabin (561) (false)))
printfn "%s" (_str (miller_rabin (563) (false)))
printfn "%s" (_str (miller_rabin (838201) (false)))
printfn "%s" (_str (miller_rabin (838207) (false)))
printfn "%s" (_str (miller_rabin (17316001) (false)))
printfn "%s" (_str (miller_rabin (17316017) (false)))
printfn "%s" (_str (miller_rabin (int 3078386641L) (false)))
printfn "%s" (_str (miller_rabin (int 3078386653L) (false)))
printfn "%s" (_str (miller_rabin (int 1713045574801L) (false)))
printfn "%s" (_str (miller_rabin (int 1713045574819L) (false)))
printfn "%s" (_str (miller_rabin (int 2779799728307L) (false)))
printfn "%s" (_str (miller_rabin (int 2779799728327L) (false)))
printfn "%s" (_str (miller_rabin (int 113850023909441L) (false)))
printfn "%s" (_str (miller_rabin (int 113850023909527L) (false)))
printfn "%s" (_str (miller_rabin (int 1275041018848804351L) (false)))
printfn "%s" (_str (miller_rabin (int 1275041018848804391L) (false)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

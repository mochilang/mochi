// Generated 2025-08-06 23:33 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec gcd (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = if a < 0 then (-a) else a
        let mutable y: int = if b < 0 then (-b) else b
        while y <> 0 do
            let t: int = ((x % y + y) % y)
            x <- y
            y <- t
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and find_mod_inverse (a: int) (m: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable m = m
    try
        if (gcd (a) (m)) <> 1 then
            failwith (((("mod inverse of " + (_str (a))) + " and ") + (_str (m))) + " does not exist")
        let mutable u1: int = 1
        let mutable u2: int = 0
        let mutable u3: int = a
        let mutable v1: int = 0
        let mutable v2: int = 1
        let mutable v3: int = m
        while v3 <> 0 do
            let q: int = u3 / v3
            let t1: int = u1 - (q * v1)
            let t2: int = u2 - (q * v2)
            let t3: int = u3 - (q * v3)
            u1 <- v1
            u2 <- v2
            u3 <- v3
            v1 <- t1
            v2 <- t2
            v3 <- t3
        let mutable res: int = ((u1 % m + m) % m)
        if res < 0 then
            res <- res + m
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (find_mod_inverse (3) (11)))
printfn "%s" (_str (find_mod_inverse (7) (26)))
printfn "%s" (_str (find_mod_inverse (11) (26)))
printfn "%s" (_str (find_mod_inverse (17) (43)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

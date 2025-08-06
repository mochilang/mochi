// Generated 2025-08-06 21:33 +0700

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
and extended_gcd (a: int) (b: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        if b = 0 then
            __ret <- unbox<int array> [|a; 1; 0|]
            raise Return
        let res: int array = extended_gcd (b) (((a % b + b) % b))
        let d: int = _idx res (0)
        let p: int = _idx res (1)
        let q: int = _idx res (2)
        let mutable x: int = q
        let mutable y: int = p - (q * (a / b))
        __ret <- unbox<int array> [|d; x; y|]
        raise Return
        __ret
    with
        | Return -> __ret
and diophantine (a: int) (b: int) (c: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    try
        let d: int = gcd (a) (b)
        if (((c % d + d) % d)) <> 0 then
            failwith ("No solution")
        let eg: int array = extended_gcd (a) (b)
        let r: int = c / d
        let mutable x: int = (_idx eg (1)) * r
        let mutable y: int = (_idx eg (2)) * r
        __ret <- unbox<int array> [|x; y|]
        raise Return
        __ret
    with
        | Return -> __ret
and diophantine_all_soln (a: int) (b: int) (c: int) (n: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    let mutable n = n
    try
        let ``base``: int array = diophantine (a) (b) (c)
        let x0: int = _idx ``base`` (0)
        let y0: int = _idx ``base`` (1)
        let d: int = gcd (a) (b)
        let p: int = a / d
        let q: int = b / d
        let mutable sols: int array array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable x: int = x0 + (i * q)
            let mutable y: int = y0 - (i * p)
            sols <- Array.append sols [|[|x; y|]|]
            i <- i + 1
        __ret <- sols
        raise Return
        __ret
    with
        | Return -> __ret
let s1: int array = diophantine (10) (6) (14)
printfn "%s" (_str (s1))
let mutable sols: int array array = diophantine_all_soln (10) (6) (14) (4)
let mutable j: int = 0
while j < (Seq.length (sols)) do
    printfn "%s" (_str (_idx sols (j)))
    j <- j + 1
printfn "%s" (_str (diophantine (391) (299) (-69)))
printfn "%s" (_str (extended_gcd (10) (6)))
printfn "%s" (_str (extended_gcd (7) (5)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

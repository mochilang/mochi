// Generated 2025-08-07 10:31 +0700

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec gcd (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        while y <> 0 do
            let mutable t: int = ((x % y + y) % y)
            x <- y
            y <- t
        if x < 0 then
            __ret <- -x
            raise Return
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
let rec pow_mod (``base``: int) (exp: int) (``mod``: int) =
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
            e <- e / 2
            b <- (((b * b) % ``mod`` + ``mod``) % ``mod``)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec rsa_factor (d: int) (e: int) (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable d = d
    let mutable e = e
    let mutable n = n
    try
        let k: int = (d * e) - 1
        let mutable p: int = 0
        let mutable q: int = 0
        let mutable g: int = 2
        try
            while (p = 0) && (g < n) do
                try
                    let mutable t: int = k
                    try
                        while (((t % 2 + 2) % 2)) = 0 do
                            try
                                t <- t / 2
                                let mutable x: int = pow_mod (g) (t) (n)
                                let mutable y: int = gcd (x - 1) (n)
                                if (x > 1) && (y > 1) then
                                    p <- y
                                    q <- n / y
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    g <- g + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        if p > q then
            __ret <- unbox<int array> [|q; p|]
            raise Return
        __ret <- unbox<int array> [|p; q|]
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_repr (rsa_factor (3) (16971) (25777)))
printfn "%s" (_repr (rsa_factor (7331) (11) (27233)))
printfn "%s" (_repr (rsa_factor (4021) (13) (17711)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

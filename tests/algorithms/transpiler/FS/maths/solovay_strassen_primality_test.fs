// Generated 2025-08-08 18:58 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
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
let mutable _seed: int = 1
let rec set_seed (s: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable s = s
    try
        _seed <- s
        __ret
    with
        | Return -> __ret
and randint (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        _seed <- int ((((((int64 _seed) * (int64 1103515245)) + (int64 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- (((_seed % ((b - a) + 1) + ((b - a) + 1)) % ((b - a) + 1))) + a
        raise Return
        __ret
    with
        | Return -> __ret
and jacobi_symbol (random_a: int) (number: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable random_a = random_a
    let mutable number = number
    try
        if (random_a = 0) || (random_a = 1) then
            __ret <- random_a
            raise Return
        random_a <- ((random_a % number + number) % number)
        let mutable t: int = 1
        while random_a <> 0 do
            while (((random_a % 2 + 2) % 2)) = 0 do
                random_a <- _floordiv random_a 2
                let r: int = ((number % 8 + 8) % 8)
                if (r = 3) || (r = 5) then
                    t <- -t
            let temp: int = random_a
            random_a <- number
            number <- temp
            if ((((random_a % 4 + 4) % 4)) = 3) && ((((number % 4 + 4) % 4)) = 3) then
                t <- -t
            random_a <- ((random_a % number + number) % number)
        if number = 1 then
            __ret <- t
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and pow_mod (``base``: int) (exp: int) (``mod``: int) =
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
                result <- int (((((int64 result) * (int64 b)) % (int64 ``mod``) + (int64 ``mod``)) % (int64 ``mod``)))
            b <- int (((((int64 b) * (int64 b)) % (int64 ``mod``) + (int64 ``mod``)) % (int64 ``mod``)))
            e <- _floordiv e 2
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and solovay_strassen (number: int) (iterations: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    let mutable iterations = iterations
    try
        if number <= 1 then
            __ret <- false
            raise Return
        if number <= 3 then
            __ret <- true
            raise Return
        let mutable i: int = 0
        while i < iterations do
            let a: int = randint (2) (number - 2)
            let x: int = jacobi_symbol (a) (number)
            let y: int = pow_mod (a) (_floordiv (number - 1) 2) (number)
            let mutable mod_x: int = ((x % number + number) % number)
            if mod_x < 0 then
                mod_x <- mod_x + number
            if (x = 0) || (y <> mod_x) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        set_seed (10)
        printfn "%s" (_str (solovay_strassen (13) (5)))
        printfn "%s" (_str (solovay_strassen (9) (10)))
        printfn "%s" (_str (solovay_strassen (17) (15)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

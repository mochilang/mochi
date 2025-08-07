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
type Keys = {
    public_key: int array
    private_key: int array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec pow2 (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable exp = exp
    try
        let mutable res: int = 1
        let mutable i: int = 0
        while i < exp do
            res <- res * 2
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let mutable seed: int = 1
let rec next_seed (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- int ((((int64 ((x * 1103515245) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        raise Return
        __ret
    with
        | Return -> __ret
let rec rand_range (min: int) (max: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable min = min
    let mutable max = max
    try
        seed <- next_seed (seed)
        __ret <- min + (((seed % (max - min) + (max - min)) % (max - min)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec gcd (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        while y <> 0 do
            let temp: int = ((x % y + y) % y)
            x <- y
            y <- temp
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
let rec mod_inverse (e: int) (phi: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable e = e
    let mutable phi = phi
    try
        let mutable t: int = 0
        let mutable newt: int = 1
        let mutable r: int = phi
        let mutable newr: int = e
        while newr <> 0 do
            let quotient: int = r / newr
            let tmp: int = newt
            newt <- t - (quotient * newt)
            t <- tmp
            let tmp_r: int = newr
            newr <- r - (quotient * newr)
            r <- tmp_r
        if r > 1 then
            __ret <- 0
            raise Return
        if t < 0 then
            t <- t + phi
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_prime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        let mutable i: int = 2
        while (i * i) <= n do
            if (((n % i + i) % i)) = 0 then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec generate_prime (bits: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable bits = bits
    try
        let min: int = pow2 (bits - 1)
        let max: int = pow2 (bits)
        let mutable p: int = rand_range (min) (max)
        if (((p % 2 + 2) % 2)) = 0 then
            p <- p + 1
        while not (is_prime (p)) do
            p <- p + 2
            if p >= max then
                p <- min + 1
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
let rec generate_key (bits: int) =
    let mutable __ret : Keys = Unchecked.defaultof<Keys>
    let mutable bits = bits
    try
        let mutable p: int = generate_prime (bits)
        let q: int = generate_prime (bits)
        let n: int = p * q
        let phi: int = (p - 1) * (q - 1)
        let mutable e: int = rand_range (2) (phi)
        while (gcd (e) (phi)) <> 1 do
            e <- e + 1
            if e >= phi then
                e <- 2
        let d: int = mod_inverse (e) (phi)
        __ret <- { public_key = [|n; e|]; private_key = [|n; d|] }
        raise Return
        __ret
    with
        | Return -> __ret
let keys: Keys = generate_key (8)
let pub: int array = keys.public_key
let priv: int array = keys.private_key
printfn "%s" (((("Public key: (" + (_str (_idx pub (0)))) + ", ") + (_str (_idx pub (1)))) + ")")
printfn "%s" (((("Private key: (" + (_str (_idx priv (0)))) + ", ") + (_str (_idx priv (1)))) + ")")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

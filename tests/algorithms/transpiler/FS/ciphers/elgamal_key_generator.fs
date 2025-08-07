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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type GCD = {
    g: int
    x: int
    y: int
}
type PublicKey = {
    key_size: int
    g: int
    e2: int
    p: int
}
type PrivateKey = {
    key_size: int
    d: int
}
type KeyPair = {
    public_key: PublicKey
    private_key: PrivateKey
}
let mutable seed: int = 123456789
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        seed <- ((((seed * 1103515245) + 12345) % 2147483647 + 2147483647) % 2147483647)
        __ret <- seed
        raise Return
        __ret
    with
        | Return -> __ret
and rand_range (min: int) (max: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable min = min
    let mutable max = max
    try
        __ret <- min + ((((rand()) % ((max - min) + 1) + ((max - min) + 1)) % ((max - min) + 1)))
        raise Return
        __ret
    with
        | Return -> __ret
and mod_pow (``base``: int) (exponent: int) (modulus: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exponent = exponent
    let mutable modulus = modulus
    try
        let mutable result: int = 1
        let mutable b: int = ((``base`` % modulus + modulus) % modulus)
        let mutable e: int = exponent
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                result <- (((result * b) % modulus + modulus) % modulus)
            e <- e / 2
            b <- (((b * b) % modulus + modulus) % modulus)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and extended_gcd (a: int) (b: int) =
    let mutable __ret : GCD = Unchecked.defaultof<GCD>
    let mutable a = a
    let mutable b = b
    try
        if b = 0 then
            __ret <- { g = a; x = 1; y = 0 }
            raise Return
        let res: GCD = extended_gcd (b) (((a % b + b) % b))
        __ret <- { g = res.g; x = res.y; y = (res.x) - ((a / b) * (res.y)) }
        raise Return
        __ret
    with
        | Return -> __ret
and mod_inverse (a: int) (m: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable m = m
    try
        let res: GCD = extended_gcd (a) (m)
        if (res.g) <> 1 then
            failwith ("inverse does not exist")
        let mutable r: int = (((res.x) % m + m) % m)
        if r < 0 then
            __ret <- r + m
            raise Return
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and pow2 (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable r: int = 1
        let mutable i: int = 0
        while i < n do
            r <- r * 2
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and is_probable_prime (n: int) (k: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    let mutable k = k
    try
        if n <= 1 then
            __ret <- false
            raise Return
        if n <= 3 then
            __ret <- true
            raise Return
        if (((n % 2 + 2) % 2)) = 0 then
            __ret <- false
            raise Return
        let mutable r: int = 0
        let mutable d: int = n - 1
        while (((d % 2 + 2) % 2)) = 0 do
            d <- d / 2
            r <- r + 1
        let mutable i: int = 0
        try
            while i < k do
                try
                    let a: int = rand_range (2) (n - 2)
                    let mutable x: int = mod_pow (a) (d) (n)
                    if (x = 1) || (x = (n - 1)) then
                        i <- i + 1
                        raise Continue
                    let mutable j: int = 1
                    let mutable found: bool = false
                    try
                        while j < r do
                            try
                                x <- mod_pow (x) (2) (n)
                                if x = (n - 1) then
                                    found <- true
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if not found then
                        __ret <- false
                        raise Return
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and generate_large_prime (bits: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable bits = bits
    try
        let min: int = pow2 (bits - 1)
        let max: int = (pow2 (bits)) - 1
        let mutable p: int = rand_range (min) (max)
        if (((p % 2 + 2) % 2)) = 0 then
            p <- p + 1
        while not (is_probable_prime (p) (5)) do
            p <- p + 2
            if p > max then
                p <- min + 1
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
and primitive_root (p: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable p = p
    try
        try
            while true do
                try
                    let g: int = rand_range (3) (p - 1)
                    if (mod_pow (g) (2) (p)) = 1 then
                        raise Continue
                    if (mod_pow (g) (p) (p)) = 1 then
                        raise Continue
                    __ret <- g
                    raise Return
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
and generate_key (key_size: int) =
    let mutable __ret : KeyPair = Unchecked.defaultof<KeyPair>
    let mutable key_size = key_size
    try
        let mutable p: int = generate_large_prime (key_size)
        let e1: int = primitive_root (p)
        let mutable d: int = rand_range (3) (p - 1)
        let e2: int = mod_inverse (mod_pow (e1) (d) (p)) (p)
        let public_key: PublicKey = { key_size = key_size; g = e1; e2 = e2; p = p }
        let private_key: PrivateKey = { key_size = key_size; d = d }
        __ret <- { public_key = public_key; private_key = private_key }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let key_size: int = 16
        let kp: KeyPair = generate_key (key_size)
        let pub: PublicKey = kp.public_key
        let priv: PrivateKey = kp.private_key
        printfn "%s" (((((((("public key: (" + (_str (pub.key_size))) + ", ") + (_str (pub.g))) + ", ") + (_str (pub.e2))) + ", ") + (_str (pub.p))) + ")")
        printfn "%s" (((("private key: (" + (_str (priv.key_size))) + ", ") + (_str (priv.d))) + ")")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

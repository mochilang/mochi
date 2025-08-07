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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec int_to_hex (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n = 0 then
            __ret <- "0"
            raise Return
        let digits: string = "0123456789abcdef"
        let mutable num: int = n
        let mutable res: string = ""
        while num > 0 do
            let d: int = ((num % 16 + 16) % 16)
            res <- (string (digits.[d])) + res
            num <- num / 16
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let mutable seed: int = 123456789
let rec rand_int () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        seed <- int ((((int64 ((1103515245 * seed) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- seed
        raise Return
        __ret
    with
        | Return -> __ret
let PRIME: int = 23
let rec mod_pow (``base``: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: int = 1
        let mutable b: int = ((``base`` % PRIME + PRIME) % PRIME)
        let mutable e: int = exp
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                result <- (((result * b) % PRIME + PRIME) % PRIME)
            b <- (((b * b) % PRIME + PRIME) % PRIME)
            e <- e / 2
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_valid_public_key (key: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable key = key
    try
        __ret <- if (key < 2) || (key > (PRIME - 2)) then false else ((mod_pow (key) ((PRIME - 1) / 2)) = 1)
        raise Return
        __ret
    with
        | Return -> __ret
let rec generate_private_key () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        __ret <- ((((rand_int()) % (PRIME - 2) + (PRIME - 2)) % (PRIME - 2))) + 2
        raise Return
        __ret
    with
        | Return -> __ret
let generator: int = 5
let alice_private: int = generate_private_key()
let alice_public: int = mod_pow (generator) (alice_private)
let bob_private: int = generate_private_key()
let bob_public: int = mod_pow (generator) (bob_private)
if not (is_valid_public_key (alice_public)) then
    failwith ("Invalid public key")
if not (is_valid_public_key (bob_public)) then
    failwith ("Invalid public key")
let alice_shared: int = mod_pow (bob_public) (alice_private)
let bob_shared: int = mod_pow (alice_public) (bob_private)
printfn "%s" (int_to_hex (alice_shared))
printfn "%s" (int_to_hex (bob_shared))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

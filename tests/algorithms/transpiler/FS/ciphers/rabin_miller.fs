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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System

let rec int_pow (``base``: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
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
let rec rand_range (low: int) (high: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable low = low
    let mutable high = high
    try
        __ret <- int ((int ((((_now()) % (high - low) + (high - low)) % (high - low)))) + low)
        raise Return
        __ret
    with
        | Return -> __ret
let rec rabin_miller (num: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable num = num
    try
        let mutable s: int = num - 1
        let mutable t: int = 0
        while (((s % 2 + 2) % 2)) = 0 do
            s <- s / 2
            t <- t + 1
        let mutable k: int = 0
        while k < 5 do
            let a: int = rand_range (2) (num - 1)
            let mutable v: int = pow_mod (a) (s) (num)
            if v <> 1 then
                let mutable i: int = 0
                while v <> (num - 1) do
                    if i = (t - 1) then
                        __ret <- false
                        raise Return
                    i <- i + 1
                    v <- (((v * v) % num + num) % num)
            k <- k + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_prime_low_num (num: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable num = num
    try
        if num < 2 then
            __ret <- false
            raise Return
        let low_primes: int array = [|2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 73; 79; 83; 89; 97; 101; 103; 107; 109; 113; 127; 131; 137; 139; 149; 151; 157; 163; 167; 173; 179; 181; 191; 193; 197; 199; 211; 223; 227; 229; 233; 239; 241; 251; 257; 263; 269; 271; 277; 281; 283; 293; 307; 311; 313; 317; 331; 337; 347; 349; 353; 359; 367; 373; 379; 383; 389; 397; 401; 409; 419; 421; 431; 433; 439; 443; 449; 457; 461; 463; 467; 479; 487; 491; 499; 503; 509; 521; 523; 541; 547; 557; 563; 569; 571; 577; 587; 593; 599; 601; 607; 613; 617; 619; 631; 641; 643; 647; 653; 659; 661; 673; 677; 683; 691; 701; 709; 719; 727; 733; 739; 743; 751; 757; 761; 769; 773; 787; 797; 809; 811; 821; 823; 827; 829; 839; 853; 857; 859; 863; 877; 881; 883; 887; 907; 911; 919; 929; 937; 941; 947; 953; 967; 971; 977; 983; 991; 997|]
        if Seq.contains num low_primes then
            __ret <- true
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (low_primes)) do
            let p: int = _idx low_primes (i)
            if (((num % p + p) % p)) = 0 then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- rabin_miller (num)
        raise Return
        __ret
    with
        | Return -> __ret
let rec generate_large_prime (keysize: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable keysize = keysize
    try
        let mutable start: int = int_pow (2) (keysize - 1)
        let mutable ``end``: int = int_pow (2) (keysize)
        while true do
            let num: int = rand_range (start) (``end``)
            if is_prime_low_num (num) then
                __ret <- num
                raise Return
        __ret
    with
        | Return -> __ret
let p: int = generate_large_prime (16)
printfn "%s" ("Prime number: " + (_str (p)))
printfn "%s" ("is_prime_low_num: " + (_str (is_prime_low_num (p))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

// Generated 2025-08-08 16:34 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let MOD: int64 = 4294967296L
let ASCII: string = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
let rec ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (ASCII)) do
            if (_substring ASCII (i) (i + 1)) = ch then
                __ret <- 32 + i
                raise Return
            i <- i + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and pow2 (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable res: int = 1
        let mutable i: int = 0
        while i < n do
            res <- res * 2
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and bit_and (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        let mutable res: int = 0
        let mutable bit: int = 1
        let mutable i: int = 0
        while i < 32 do
            if ((((x % 2 + 2) % 2)) = 1) && ((((y % 2 + 2) % 2)) = 1) then
                res <- res + bit
            x <- _floordiv x 2
            y <- _floordiv y 2
            bit <- bit * 2
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and bit_or (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        let mutable res: int = 0
        let mutable bit: int = 1
        let mutable i: int = 0
        while i < 32 do
            let abit: int = ((x % 2 + 2) % 2)
            let bbit: int = ((y % 2 + 2) % 2)
            if (abit = 1) || (bbit = 1) then
                res <- res + bit
            x <- _floordiv x 2
            y <- _floordiv y 2
            bit <- bit * 2
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and bit_xor (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        let mutable res: int = 0
        let mutable bit: int = 1
        let mutable i: int = 0
        while i < 32 do
            let abit: int = ((x % 2 + 2) % 2)
            let bbit: int = ((y % 2 + 2) % 2)
            if ((abit = 1) && (bbit = 0)) || ((abit = 0) && (bbit = 1)) then
                res <- res + bit
            x <- _floordiv x 2
            y <- _floordiv y 2
            bit <- bit * 2
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and bit_not (a: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    try
        __ret <- int ((MOD - (int64 1)) - (int64 a))
        raise Return
        __ret
    with
        | Return -> __ret
and rotate_left (n: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    let mutable b = b
    try
        let left: int64 = (((int64 (n * (pow2 (b)))) % MOD + MOD) % MOD)
        let right: int = _floordiv n (pow2 (32 - b))
        __ret <- int ((((left + (int64 right)) % MOD + MOD) % MOD))
        raise Return
        __ret
    with
        | Return -> __ret
and to_hex32 (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let digits: string = "0123456789abcdef"
        let mutable num: int = n
        let mutable s: string = ""
        if num = 0 then
            s <- "0"
        while num > 0 do
            let mutable d: int = ((num % 16 + 16) % 16)
            s <- (_substring digits (d) (d + 1)) + s
            num <- _floordiv num 16
        while (String.length (s)) < 8 do
            s <- "0" + s
        if (String.length (s)) > 8 then
            s <- _substring s ((String.length (s)) - 8) (String.length (s))
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and sha1 (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    try
        let mutable bytes: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (String.length (message)) do
            bytes <- Array.append bytes [|(ord (_substring message (i) (i + 1)))|]
            i <- i + 1
        bytes <- Array.append bytes [|128|]
        while (((((Seq.length (bytes)) + 8) % 64 + 64) % 64)) <> 0 do
            bytes <- Array.append bytes [|0|]
        let bit_len: int = (String.length (message)) * 8
        let mutable len_bytes: int array = unbox<int array> [|0; 0; 0; 0; 0; 0; 0; 0|]
        let mutable bl: int = bit_len
        let mutable k: int = 7
        while k >= 0 do
            len_bytes.[k] <- ((bl % 256 + 256) % 256)
            bl <- _floordiv bl 256
            k <- k - 1
        let mutable j: int = 0
        while j < 8 do
            bytes <- Array.append bytes [|(_idx len_bytes (j))|]
            j <- j + 1
        let mutable blocks: int array array = Array.empty<int array>
        let mutable pos: int = 0
        while pos < (Seq.length (bytes)) do
            let mutable block: int array = Array.empty<int>
            let mutable j2: int = 0
            while j2 < 64 do
                block <- Array.append block [|(_idx bytes (pos + j2))|]
                j2 <- j2 + 1
            blocks <- Array.append blocks [|block|]
            pos <- pos + 64
        let mutable h0: int = 1732584193
        let mutable h1: int = int 4023233417L
        let mutable h2: int = int 2562383102L
        let mutable h3: int = 271733878
        let mutable h4: int = int 3285377520L
        let mutable bindex: int = 0
        while bindex < (Seq.length (blocks)) do
            let mutable block: int array = _idx blocks (bindex)
            let mutable w: int array = Array.empty<int>
            let mutable t: int = 0
            while t < 16 do
                let j3: int = t * 4
                let word: int = ((((((_idx block (j3)) * 256) + (_idx block (j3 + 1))) * 256) + (_idx block (j3 + 2))) * 256) + (_idx block (j3 + 3))
                w <- Array.append w [|word|]
                t <- t + 1
            while t < 80 do
                let tmp: int = bit_xor (bit_xor (bit_xor (_idx w (t - 3)) (_idx w (t - 8))) (_idx w (t - 14))) (_idx w (t - 16))
                w <- Array.append w [|(rotate_left (tmp) (1))|]
                t <- t + 1
            let mutable a: int = h0
            let mutable b: int = h1
            let mutable c: int = h2
            let mutable d: int = h3
            let mutable e: int = h4
            let mutable i2: int = 0
            while i2 < 80 do
                let mutable f: int = 0
                let mutable kconst: int = 0
                if i2 < 20 then
                    f <- bit_or (bit_and (b) (c)) (bit_and (bit_not (b)) (d))
                    kconst <- 1518500249
                else
                    if i2 < 40 then
                        f <- bit_xor (bit_xor (b) (c)) (d)
                        kconst <- 1859775393
                    else
                        if i2 < 60 then
                            f <- bit_or (bit_or (bit_and (b) (c)) (bit_and (b) (d))) (bit_and (c) (d))
                            kconst <- int 2400959708L
                        else
                            f <- bit_xor (bit_xor (b) (c)) (d)
                            kconst <- int 3395469782L
                let temp: int64 = (((int64 (((((rotate_left (a) (5)) + f) + e) + kconst) + (_idx w (i2)))) % MOD + MOD) % MOD)
                e <- d
                d <- c
                c <- rotate_left (b) (30)
                b <- a
                a <- int temp
                i2 <- i2 + 1
            h0 <- int ((((int64 (h0 + a)) % MOD + MOD) % MOD))
            h1 <- int ((((int64 (h1 + b)) % MOD + MOD) % MOD))
            h2 <- int ((((int64 (h2 + c)) % MOD + MOD) % MOD))
            h3 <- int ((((int64 (h3 + d)) % MOD + MOD) % MOD))
            h4 <- int ((((int64 (h4 + e)) % MOD + MOD) % MOD))
            bindex <- bindex + 1
        __ret <- ((((to_hex32 (h0)) + (to_hex32 (h1))) + (to_hex32 (h2))) + (to_hex32 (h3))) + (to_hex32 (h4))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (sha1 ("Test String"))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

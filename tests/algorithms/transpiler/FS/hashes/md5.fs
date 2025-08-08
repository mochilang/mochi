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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
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
let rec to_little_endian (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        if (String.length (s)) <> 32 then
            failwith ("Input must be of length 32")
        __ret <- (((_substring s (24) (32)) + (_substring s (16) (24))) + (_substring s (8) (16))) + (_substring s (0) (8))
        raise Return
        __ret
    with
        | Return -> __ret
let rec int_to_bits (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable bits: string = ""
        let mutable num: int = n
        while num > 0 do
            bits <- (_str (((num % 2 + 2) % 2))) + bits
            num <- _floordiv num 2
        while (String.length (bits)) < width do
            bits <- "0" + bits
        if (String.length (bits)) > width then
            bits <- _substring bits ((String.length (bits)) - width) (String.length (bits))
        __ret <- bits
        raise Return
        __ret
    with
        | Return -> __ret
let rec bits_to_int (bits: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable bits = bits
    try
        let mutable num: int = 0
        let mutable i: int = 0
        while i < (String.length (bits)) do
            if (_substring bits (i) (i + 1)) = "1" then
                num <- (num * 2) + 1
            else
                num <- num * 2
            i <- i + 1
        __ret <- num
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_hex (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let digits: string = "0123456789abcdef"
        if n = 0 then
            __ret <- "0"
            raise Return
        let mutable num: int = n
        let mutable s: string = ""
        while num > 0 do
            let mutable d: int = ((num % 16 + 16) % 16)
            s <- (_substring digits (d) (d + 1)) + s
            num <- _floordiv num 16
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec reformat_hex (i: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable i = i
    try
        if i < 0 then
            failwith ("Input must be non-negative")
        let mutable hex: string = to_hex (i)
        while (String.length (hex)) < 8 do
            hex <- "0" + hex
        if (String.length (hex)) > 8 then
            hex <- _substring hex ((String.length (hex)) - 8) (String.length (hex))
        let mutable le: string = ""
        let mutable j: int = (String.length (hex)) - 2
        while j >= 0 do
            le <- le + (_substring hex (j) (j + 2))
            j <- j - 2
        __ret <- le
        raise Return
        __ret
    with
        | Return -> __ret
let rec preprocess (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    try
        let mutable bit_string: string = ""
        let mutable i: int = 0
        while i < (String.length (message)) do
            let ch: string = _substring message (i) (i + 1)
            bit_string <- bit_string + (int_to_bits (ord (ch)) (8))
            i <- i + 1
        let start_len: string = int_to_bits (String.length (bit_string)) (64)
        bit_string <- bit_string + "1"
        while ((((String.length (bit_string)) % 512 + 512) % 512)) <> 448 do
            bit_string <- bit_string + "0"
        bit_string <- (bit_string + (to_little_endian (_substring start_len (32) (64)))) + (to_little_endian (_substring start_len (0) (32)))
        __ret <- bit_string
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_block_words (bit_string: string) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable bit_string = bit_string
    try
        if ((((String.length (bit_string)) % 512 + 512) % 512)) <> 0 then
            failwith ("Input must have length that's a multiple of 512")
        let mutable blocks: int array array = Array.empty<int array>
        let mutable pos: int = 0
        while pos < (String.length (bit_string)) do
            let mutable block: int array = Array.empty<int>
            let mutable i: int = 0
            while i < 512 do
                let part: string = _substring bit_string (pos + i) ((pos + i) + 32)
                let word: int = bits_to_int (to_little_endian (part))
                block <- Array.append block [|word|]
                i <- i + 32
            blocks <- Array.append blocks [|block|]
            pos <- pos + 512
        __ret <- blocks
        raise Return
        __ret
    with
        | Return -> __ret
let rec bit_and (a: int) (b: int) =
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
let rec bit_or (a: int) (b: int) =
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
let rec bit_xor (a: int) (b: int) =
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
            if ((((abit + bbit) % 2 + 2) % 2)) = 1 then
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
let rec not_32 (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    try
        if i < 0 then
            failwith ("Input must be non-negative")
        __ret <- int (4294967295L - (int64 i))
        raise Return
        __ret
    with
        | Return -> __ret
let rec sum_32 (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- int ((((int64 (a + b)) % MOD + MOD) % MOD))
        raise Return
        __ret
    with
        | Return -> __ret
let rec lshift (num: int) (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    let mutable k = k
    try
        let mutable result: int64 = (((int64 num) % MOD + MOD) % MOD)
        let mutable i: int = 0
        while i < k do
            result <- (((result * (int64 2)) % MOD + MOD) % MOD)
            i <- i + 1
        __ret <- int result
        raise Return
        __ret
    with
        | Return -> __ret
let rec rshift (num: int) (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    let mutable k = k
    try
        let mutable result: int = num
        let mutable i: int = 0
        while i < k do
            result <- _floordiv result 2
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec left_rotate_32 (i: int) (shift: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    let mutable shift = shift
    try
        if i < 0 then
            failwith ("Input must be non-negative")
        if shift < 0 then
            failwith ("Shift must be non-negative")
        let left: int = lshift (i) (shift)
        let right: int = rshift (i) (32 - shift)
        __ret <- int ((((int64 (left + right)) % MOD + MOD) % MOD))
        raise Return
        __ret
    with
        | Return -> __ret
let rec md5_me (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    try
        let mutable bit_string: string = preprocess (message)
        let added_consts: int64 array = unbox<int64 array> [|3614090360L; 3905402710L; int64 (606105819); 3250441966L; 4118548399L; int64 (1200080426); 2821735955L; 4249261313L; int64 (1770035416); 2336552879L; 4294925233L; 2304563134L; int64 (1804603682); 4254626195L; 2792965006L; int64 (1236535329); 4129170786L; 3225465664L; int64 (643717713); 3921069994L; 3593408605L; int64 (38016083); 3634488961L; 3889429448L; int64 (568446438); 3275163606L; 4107603335L; int64 (1163531501); 2850285829L; 4243563512L; int64 (1735328473); 2368359562L; 4294588738L; 2272392833L; int64 (1839030562); 4259657740L; 2763975236L; int64 (1272893353); 4139469664L; 3200236656L; int64 (681279174); 3936430074L; 3572445317L; int64 (76029189); 3654602809L; 3873151461L; int64 (530742520); 3299628645L; 4096336452L; int64 (1126891415); 2878612391L; 4237533241L; int64 (1700485571); 2399980690L; 4293915773L; 2240044497L; int64 (1873313359); 4264355552L; 2734768916L; int64 (1309151649); 4149444226L; 3174756917L; int64 (718787259); 3951481745L|]
        let shift_amounts: int array = unbox<int array> [|7; 12; 17; 22; 7; 12; 17; 22; 7; 12; 17; 22; 7; 12; 17; 22; 5; 9; 14; 20; 5; 9; 14; 20; 5; 9; 14; 20; 5; 9; 14; 20; 4; 11; 16; 23; 4; 11; 16; 23; 4; 11; 16; 23; 4; 11; 16; 23; 6; 10; 15; 21; 6; 10; 15; 21; 6; 10; 15; 21; 6; 10; 15; 21|]
        let mutable a0: int = 1732584193
        let mutable b0: int = int 4023233417L
        let mutable c0: int = int 2562383102L
        let mutable d0: int = 271733878
        let mutable blocks: int array array = get_block_words (bit_string)
        let mutable bi: int = 0
        while bi < (Seq.length (blocks)) do
            let mutable block: int array = _idx blocks (bi)
            let mutable a: int = a0
            let mutable b: int = b0
            let mutable c: int = c0
            let mutable d: int = d0
            let mutable i: int = 0
            while i < 64 do
                let mutable f: int = 0
                let mutable g: int = 0
                if i <= 15 then
                    f <- bit_xor (d) (bit_and (b) (bit_xor (c) (d)))
                    g <- i
                else
                    if i <= 31 then
                        f <- bit_xor (c) (bit_and (d) (bit_xor (b) (c)))
                        g <- ((((5 * i) + 1) % 16 + 16) % 16)
                    else
                        if i <= 47 then
                            f <- bit_xor (bit_xor (b) (c)) (d)
                            g <- ((((3 * i) + 5) % 16 + 16) % 16)
                        else
                            f <- bit_xor (c) (bit_or (b) (not_32 (d)))
                            g <- (((7 * i) % 16 + 16) % 16)
                f <- sum_32 (f) (a)
                f <- sum_32 (f) (int (_idx added_consts (i)))
                f <- sum_32 (f) (_idx block (g))
                let rotated: int = left_rotate_32 (f) (_idx shift_amounts (i))
                let new_b: int = sum_32 (b) (rotated)
                a <- d
                d <- c
                c <- b
                b <- new_b
                i <- i + 1
            a0 <- sum_32 (a0) (a)
            b0 <- sum_32 (b0) (b)
            c0 <- sum_32 (c0) (c)
            d0 <- sum_32 (d0) (d)
            bi <- bi + 1
        let digest: string = (((reformat_hex (a0)) + (reformat_hex (b0))) + (reformat_hex (c0))) + (reformat_hex (d0))
        __ret <- digest
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

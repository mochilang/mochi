// Generated 2025-07-30 21:41 +0700

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
let rec xor (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: int = 0
        let mutable bit: int = 1
        let mutable x: int = a
        let mutable y: int = b
        while (x > 0) || (y > 0) do
            let abit: int = ((x % 2 + 2) % 2)
            let bbit: int = ((y % 2 + 2) % 2)
            if abit <> bbit then
                res <- res + bit
            x <- x / 2
            y <- y / 2
            bit <- bit * 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and rshift (x: int) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    let mutable n = n
    try
        let mutable v: int = x
        let mutable i: int = 0
        while i < n do
            v <- v / 2
            i <- i + 1
        __ret <- v
        raise Return
        __ret
    with
        | Return -> __ret
and ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        let mutable idx: int = upper.IndexOf(ch)
        if idx >= 0 then
            __ret <- 65 + idx
            raise Return
        idx <- int (lower.IndexOf(ch))
        if idx >= 0 then
            __ret <- 97 + idx
            raise Return
        if ch = " " then
            __ret <- 32
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and toHex (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let digits: string = "0123456789ABCDEF"
        if n = 0 then
            __ret <- "0"
            raise Return
        let mutable v: int = n
        let mutable out: string = ""
        while v > 0 do
            let d: int = ((v % 16 + 16) % 16)
            out <- (digits.Substring(d, (d + 1) - d)) + out
            v <- v / 16
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and crc32Table () =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    try
        let mutable table: int array = [||]
        let mutable i: int = 0
        while i < 256 do
            let mutable word: int = i
            let mutable j: int = 0
            while j < 8 do
                if (((word % 2 + 2) % 2)) = 1 then
                    word <- xor (rshift word 1) (int 3988292384L)
                else
                    word <- rshift word 1
                j <- j + 1
            table <- Array.append table [|word|]
            i <- i + 1
        __ret <- table
        raise Return
        __ret
    with
        | Return -> __ret
let table: int array = crc32Table()
let rec crc32 (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable crc: int = (int 4294967295L)
        let mutable i: int = 0
        while i < (String.length s) do
            let c: int = ord (s.Substring(i, (i + 1) - i))
            let idx: int = xor (((crc % 256 + 256) % 256)) c
            crc <- xor (table.[idx]) (rshift crc 8)
            i <- i + 1
        __ret <- (int 4294967295L) - crc
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let s: string = "The quick brown fox jumps over the lazy dog"
        let result: int = crc32 s
        let hex: string = (toHex result).ToLower()
        printfn "%s" hex
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

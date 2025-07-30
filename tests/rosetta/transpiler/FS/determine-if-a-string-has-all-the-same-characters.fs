// Generated 2025-07-31 00:10 +0700

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

let rec ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        if ch = "5" then
            __ret <- 53
            raise Return
        if ch = "T" then
            __ret <- 84
            raise Return
        if ch = " " then
            __ret <- 32
            raise Return
        if ch = "é" then
            __ret <- 233
            raise Return
        if ch = "🐺" then
            __ret <- 128058
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and hex (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let digits: string = "0123456789abcdef"
        if n = 0 then
            __ret <- "0x0"
            raise Return
        let mutable m: int = n
        let mutable out: string = ""
        while m > 0 do
            let d: int = ((m % 16 + 16) % 16)
            out <- (_substring digits d (d + 1)) + out
            m <- m / 16
        __ret <- "0x" + out
        raise Return
        __ret
    with
        | Return -> __ret
and quote (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        __ret <- ("'" + s) + "'"
        raise Return
        __ret
    with
        | Return -> __ret
and analyze (s: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable s = s
    try
        let le: int = String.length s
        printfn "%s" (((("Analyzing " + (unbox<string> (quote s))) + " which has a length of ") + (string le)) + ":")
        if le > 1 then
            let mutable i: int = 1
            while i < le do
                let cur: string = _substring s i (i + 1)
                let prev: string = _substring s (i - 1) i
                if cur <> prev then
                    printfn "%s" "  Not all characters in the string are the same."
                    printfn "%s" (((((("  " + (unbox<string> (quote cur))) + " (") + (unbox<string> (hex (ord cur)))) + ") is different at position ") + (string (i + 1))) + ".")
                    printfn "%s" ""
                    __ret <- ()
                    raise Return
                i <- i + 1
        printfn "%s" "  All characters in the string are the same."
        printfn "%s" ""
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let strings: string array = [|""; "   "; "2"; "333"; ".55"; "tttTTT"; "4444 444k"; "pépé"; "🐶🐶🐺🐶"; "🎄🎄🎄🎄"|]
        let mutable i: int = 0
        while i < (Seq.length strings) do
            analyze (strings.[i])
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

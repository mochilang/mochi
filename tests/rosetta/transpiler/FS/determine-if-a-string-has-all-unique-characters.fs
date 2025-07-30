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

let rec indexOf3 (s: string) (ch: string) (start: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    let mutable start = start
    try
        let mutable i: int = start
        while i < (String.length s) do
            if (_substring s i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let digits: string = "0123456789"
        let mutable idx: int = indexOf3 digits ch 0
        if idx >= 0 then
            __ret <- 48 + idx
            raise Return
        if ch = "X" then
            __ret <- 88
            raise Return
        if ch = "é" then
            __ret <- 233
            raise Return
        if ch = "😍" then
            __ret <- 128525
            raise Return
        if ch = "🐡" then
            __ret <- 128033
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
and analyze (s: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable s = s
    try
        let le: int = String.length s
        printfn "%s" (((("Analyzing \"" + s) + "\" which has a length of ") + (string le)) + ":")
        if le > 1 then
            let mutable i: int = 0
            while i < (le - 1) do
                let mutable j: int = i + 1
                while j < le do
                    if (_substring s j (j + 1)) = (_substring s i (i + 1)) then
                        let ch: string = _substring s i (i + 1)
                        printfn "%s" "  Not all characters in the string are unique."
                        printfn "%s" (((((((("  '" + ch) + "' (0x") + (unbox<string> ((toHex (ord ch)).ToLower()))) + ") is duplicated at positions ") + (string (i + 1))) + " and ") + (string (j + 1))) + ".\n")
                        __ret <- ()
                        raise Return
                    j <- j + 1
                i <- i + 1
        printfn "%s" "  All characters in the string are unique.\n"
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let strings: string array = [|""; "."; "abcABC"; "XYZ ZYX"; "1234567890ABCDEFGHIJKLMN0PQRSTUVWXYZ"; "01234567890ABCDEFGHIJKLMN0PQRSTUVWXYZ0X"; "hétérogénéité"; "🎆🎃🎇🎈"; "😍😀🙌💃😍🙌"; "🐠🐟🐡🦈🐬🐳🐋🐡"|]
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

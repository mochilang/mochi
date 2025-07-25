// Generated 2025-07-26 04:38 +0700

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
let rec indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length s) do
            if (s.Substring(i, (i + 1) - i)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and fmt3 (x: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    try
        let mutable y: float = (float (int ((x * 1000.0) + 0.5))) / 1000.0
        let mutable s: string = string y
        let mutable dot: int = indexOf s "."
        if dot = (0 - 1) then
            s <- s + ".000"
        else
            let mutable decs: int = ((String.length s) - dot) - 1
            if decs > 3 then
                s <- s.Substring(0, (dot + 4) - 0)
            else
                while decs < 3 do
                    s <- s + "0"
                    decs <- decs + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and pad (s: string) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable width = width
    try
        let mutable out: string = s
        while (String.length out) < width do
            out <- " " + out
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and smaSeries (xs: float array) (period: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    let mutable period = period
    try
        let mutable res: float array = [||]
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (int (Array.length xs)) do
            sum <- sum + (float (xs.[i]))
            if i >= period then
                sum <- sum - (float (xs.[i - period]))
            let mutable denom: int = i + 1
            if denom > period then
                denom <- period
            res <- Array.append res [|sum / (float denom)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable xs: float array = [|1.0; 2.0; 3.0; 4.0; 5.0; 5.0; 4.0; 3.0; 2.0; 1.0|]
        let mutable sma3: float array = smaSeries xs 3
        let mutable sma5: float array = smaSeries xs 5
        printfn "%s" "x       sma3   sma5"
        let mutable i: int = 0
        while i < (int (Array.length xs)) do
            let line: string = ((((unbox<string> (pad (unbox<string> (fmt3 (float (xs.[i])))) 5)) + "  ") + (unbox<string> (pad (unbox<string> (fmt3 (float (sma3.[i])))) 5))) + "  ") + (unbox<string> (pad (unbox<string> (fmt3 (float (sma5.[i])))) 5))
            printfn "%s" line
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

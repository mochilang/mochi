// Generated 2025-07-28 07:48 +0700

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

let rec gcd (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        if x < 0 then
            x <- -x
        let mutable y: int = b
        if y < 0 then
            y <- -y
        while y <> 0 do
            let t: int = ((x % y + y) % y)
            x <- y
            y <- t
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and parseRational (s: string) =
    let mutable __ret : Map<string, int> = Unchecked.defaultof<Map<string, int>>
    let mutable s = s
    try
        let mutable intPart: int = 0
        let mutable fracPart: int = 0
        let mutable denom: int = 1
        let mutable afterDot: bool = false
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = _substring s i (i + 1)
            if ch = "." then
                afterDot <- true
            else
                let d: int = (int ch) - (int "0")
                if not afterDot then
                    intPart <- (intPart * 10) + d
                else
                    fracPart <- (fracPart * 10) + d
                    denom <- denom * 10
            i <- i + 1
        let mutable num: int = (intPart * denom) + fracPart
        let g: int = gcd num denom
        __ret <- Map.ofList [("num", int (num / g)); ("den", int (denom / g))]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let inputs: string array = [|"0.9054054"; "0.518518"; "0.75"|]
        for s in inputs do
            let r: Map<string, int> = parseRational s
            printfn "%s" ((((s + " = ") + (string (r.["num"] |> unbox<int>))) + "/") + (string (r.["den"] |> unbox<int>)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

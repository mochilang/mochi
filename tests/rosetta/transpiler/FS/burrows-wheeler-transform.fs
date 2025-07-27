// Generated 2025-07-27 15:57 +0700

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

let stx: string = "\x02"
let etx: string = "\x03"
let rec contains (s: string) (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length s) do
            if (_substring s i (i + 1)) = ch then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and sortStrings (xs: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    try
        let mutable arr: string array = xs
        let mutable n: int = Array.length arr
        let mutable i: int = 0
        while i < n do
            let mutable j: int = 0
            while j < (n - 1) do
                if (arr.[j]) > (arr.[j + 1]) then
                    let tmp: string = arr.[j]
                    arr.[j] <- arr.[j + 1]
                    arr.[j + 1] <- tmp
                j <- j + 1
            i <- i + 1
        __ret <- unbox<string array> arr
        raise Return
        __ret
    with
        | Return -> __ret
and bwt (s: string) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable s = s
    try
        if (s.Contains(stx)) || (s.Contains(etx)) then
            __ret <- unbox<Map<string, obj>> (Map.ofList [("err", box true); ("res", box "")])
            raise Return
        s <- (stx + s) + etx
        let le: int = String.length s
        let mutable table: string array = [||]
        let mutable i: int = 0
        while i < le do
            let rot: string = (_substring s i le) + (_substring s 0 i)
            table <- unbox<string array> (Array.append table [|rot|])
            i <- i + 1
        table <- sortStrings table
        let mutable last: string = ""
        i <- 0
        while i < le do
            last <- last + (_substring (table.[i]) (le - 1) le)
            i <- i + 1
        __ret <- unbox<Map<string, obj>> (Map.ofList [("err", box false); ("res", box last)])
        raise Return
        __ret
    with
        | Return -> __ret
and ibwt (r: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable r = r
    try
        let le: int = String.length r
        let mutable table: string array = [||]
        let mutable i: int = 0
        while i < le do
            table <- unbox<string array> (Array.append table [|""|])
            i <- i + 1
        let mutable n: int = 0
        while n < le do
            i <- 0
            while i < le do
                table.[i] <- (_substring r i (i + 1)) + (unbox<string> (table.[i]))
                i <- i + 1
            table <- sortStrings table
            n <- n + 1
        i <- 0
        while i < le do
            if (_substring (table.[i]) (le - 1) le) = etx then
                __ret <- _substring (table.[i]) 1 (le - 1)
                raise Return
            i <- i + 1
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
and makePrintable (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = _substring s i (i + 1)
            if ch = stx then
                out <- out + "^"
            else
                if ch = etx then
                    out <- out + "|"
                else
                    out <- out + ch
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let examples: string array = [|"banana"; "appellee"; "dogwood"; "TO BE OR NOT TO BE OR WANT TO BE OR NOT?"; "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES"; "\x02ABC\x03"|]
        for t in examples do
            printfn "%s" (makePrintable (unbox<string> t))
            let res: Map<string, obj> = bwt (unbox<string> t)
            if unbox<bool> (res.["err"]) then
                printfn "%s" " --> ERROR: String can't contain STX or ETX"
                printfn "%s" " -->"
            else
                let enc: string = unbox<string> (res.["res"])
                printfn "%s" (" --> " + (unbox<string> (makePrintable enc)))
                let r: string = ibwt enc
                printfn "%s" (" --> " + r)
            printfn "%s" ""
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

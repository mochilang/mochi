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

let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec padLeft (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable s: string = string n
        while (String.length s) < width do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec squeeze (s: string) (ch: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable out: string = ""
        let mutable prev: bool = false
        let mutable i: int = 0
        while i < (String.length s) do
            let c: string = _substring s i (i + 1)
            if c = ch then
                if not prev then
                    out <- out + c
                    prev <- true
            else
                out <- out + c
                prev <- false
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let strings: string array = [|""; "\"If I were two-faced, would I be wearing this one?\" --- Abraham Lincoln "; "..1111111111111111111111111111111111111111111111111111111111111117777888"; "I never give 'em hell, I just tell the truth, and they think it's hell. "; "                                                   ---  Harry S Truman  "; "The better the 4-wheel drive, the further you'll be from help when ya get stuck!"; "headmistressship"; "aardvark"; "😍😀🙌💃😍😍😍🙌"|]
let chars: string array array = [|[|" "|]; [|"-"|]; [|"7"|]; [|"."|]; [|" "; "-"; "r"|]; [|"e"|]; [|"s"|]; [|"a"|]; [|"😍"|]|]
let mutable i: int = 0
while i < (Seq.length strings) do
    let mutable j: int = 0
    let mutable s: string = strings.[i]
    while j < (Seq.length (chars.[i])) do
        let c: string = (chars.[i]).[j]
        let ss: string = squeeze s c
        printfn "%s" (("specified character = '" + c) + "'")
        printfn "%s" (((("original : length = " + (unbox<string> (padLeft (String.length s) 2))) + ", string = «««") + s) + "»»»")
        printfn "%s" (((("squeezed : length = " + (unbox<string> (padLeft (String.length ss) 2))) + ", string = «««") + ss) + "»»»")
        printfn "%s" ""
        j <- j + 1
    i <- i + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

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

let rec collapse (s: string) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable s = s
    try
        let mutable i: int = 0
        let mutable prev: string = ""
        let mutable res: string = ""
        let mutable orig: int = String.length s
        while i < (String.length s) do
            let ch: string = _substring s i (i + 1)
            if ch <> prev then
                res <- res + ch
                prev <- ch
            i <- i + 1
        __ret <- [|box res; box orig; box String.length res|]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let strings: string array = [|""; "\"If I were two-faced, would I be wearing this one?\" --- Abraham Lincoln "; "..111111111111111111111111111111111111111111111111111111111111111777888"; "I never give 'em hell, I just tell the truth, and they think it's hell. "; "                                                   ---  Harry S Truman "; "The better the 4-wheel drive, the further you'll be from help when ya get stuck!"; "headmistressship"; "aardvark"; "😍😀🙌💃😍😍😍🙌"|]
        let mutable idx: int = 0
        while idx < (Seq.length strings) do
            let s: string = strings.[idx]
            let r: obj array = collapse s
            let cs: obj = r.[0]
            let olen: obj = r.[1]
            let clen: obj = r.[2]
            printfn "%s" (((("original : length = " + (string olen)) + ", string = «««") + s) + "»»»")
            printfn "%s" (((("collapsed: length = " + (string clen)) + ", string = «««") + (unbox<string> cs)) + "»»»\n")
            idx <- idx + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

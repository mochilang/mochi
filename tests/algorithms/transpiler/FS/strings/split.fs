// Generated 2025-08-11 15:32 +0700

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

let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec split_with_sep (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable parts: string array = Array.empty<string>
        let mutable last: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s i (i + 1)
            if ch = sep then
                parts <- Array.append parts [|(_substring s last i)|]
                last <- i + 1
            if (i + 1) = (String.length (s)) then
                parts <- Array.append parts [|(_substring s last (i + 1))|]
            i <- i + 1
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
let rec split (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        __ret <- split_with_sep (s) (" ")
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (split_with_sep ("apple#banana#cherry#orange") ("#")))
printfn "%s" (_str (split ("Hello there")))
printfn "%s" (_str (split_with_sep ("11/22/63") ("/")))
printfn "%s" (_str (split_with_sep ("12:43:39") (":")))
printfn "%s" (_str (split_with_sep (";abbb;;c;") (";")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

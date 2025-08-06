// Generated 2025-08-06 20:48 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec contains (words: string array) (target: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable words = words
    let mutable target = target
    try
        for w in Seq.map string (words) do
            if w = target then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and backtrack (s: string) (word_dict: string array) (start: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable word_dict = word_dict
    let mutable start = start
    try
        if start = (String.length(s)) then
            __ret <- true
            raise Return
        let mutable ``end``: int = start + 1
        while ``end`` <= (String.length(s)) do
            let substr: string = _substring s start ``end``
            if (contains (word_dict) (substr)) && (backtrack (s) (word_dict) (``end``)) then
                __ret <- true
                raise Return
            ``end`` <- ``end`` + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and word_break (s: string) (word_dict: string array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable word_dict = word_dict
    try
        __ret <- backtrack (s) (word_dict) (0)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (word_break ("leetcode") (unbox<string array> [|"leet"; "code"|])))
printfn "%s" (_str (word_break ("applepenapple") (unbox<string array> [|"apple"; "pen"|])))
printfn "%s" (_str (word_break ("catsandog") (unbox<string array> [|"cats"; "dog"; "sand"; "and"; "cat"|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

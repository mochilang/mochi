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
let lowercase: string = "abcdefghijklmnopqrstuvwxyz"
let uppercase: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec index_of (s: string) (c: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = c then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_alpha (c: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable c = c
    try
        __ret <- ((index_of (lowercase) (c)) >= 0) || ((index_of (uppercase) (c)) >= 0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_upper (c: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    try
        let idx: int = index_of (lowercase) (c)
        if idx >= 0 then
            __ret <- _substring uppercase idx (idx + 1)
            raise Return
        __ret <- c
        raise Return
        __ret
    with
        | Return -> __ret
let rec wave (txt: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable txt = txt
    try
        let mutable result: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (String.length (txt)) do
            let ch: string = _substring txt i (i + 1)
            if is_alpha (ch) then
                let prefix: string = _substring txt 0 i
                let suffix: string = _substring txt (i + 1) (String.length (txt))
                result <- Array.append result [|((prefix + (to_upper (ch))) + suffix)|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (wave ("cat")))
printfn "%s" (_str (wave ("one")))
printfn "%s" (_str (wave ("book")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

// Generated 2025-08-11 15:32 +0700

exception Break
exception Continue

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
let VOWELS: string = "aeiou"
let rec strip (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable start: int = 0
        let mutable ``end``: int = String.length (s)
        while (start < ``end``) && ((_substring s start (start + 1)) = " ") do
            start <- start + 1
        while (``end`` > start) && ((_substring s (``end`` - 1) ``end``) = " ") do
            ``end`` <- ``end`` - 1
        __ret <- _substring s start ``end``
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_vowel (c: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (String.length (VOWELS)) do
            if c = (_substring VOWELS i (i + 1)) then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec pig_latin (word: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable word = word
    try
        let trimmed: string = strip (word)
        if (String.length (trimmed)) = 0 then
            __ret <- ""
            raise Return
        let w: string = trimmed.ToLower()
        let first: string = _substring w 0 1
        if is_vowel (first) then
            __ret <- w + "way"
            raise Return
        let mutable i: int = 0
        try
            while i < (String.length (w)) do
                try
                    let ch: string = _substring w i (i + 1)
                    if is_vowel (ch) then
                        raise Break
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- ((_substring w i (String.length (w))) + (_substring w 0 i)) + "ay"
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" ("pig_latin('friends') = " + (pig_latin ("friends")))
printfn "%s" ("pig_latin('smile') = " + (pig_latin ("smile")))
printfn "%s" ("pig_latin('eat') = " + (pig_latin ("eat")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

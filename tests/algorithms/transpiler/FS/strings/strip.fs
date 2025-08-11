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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec contains (chars: string) (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable chars = chars
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (chars)) do
            if (string (chars.[i])) = ch then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and substring (s: string) (start: int) (``end``: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        let mutable res: string = ""
        let mutable i: int = start
        while i < ``end`` do
            res <- res + (string (s.[i]))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and strip_chars (user_string: string) (characters: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable user_string = user_string
    let mutable characters = characters
    try
        let mutable start: int = 0
        let mutable ``end``: int = String.length (user_string)
        while (start < ``end``) && (contains (characters) (string (user_string.[start]))) do
            start <- start + 1
        while (``end`` > start) && (contains (characters) (string (user_string.[``end`` - 1]))) do
            ``end`` <- ``end`` - 1
        __ret <- substring (user_string) (start) (``end``)
        raise Return
        __ret
    with
        | Return -> __ret
and strip (user_string: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable user_string = user_string
    try
        __ret <- strip_chars (user_string) (" \t\n\r")
        raise Return
        __ret
    with
        | Return -> __ret
and test_strip () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (strip ("   hello   ")) <> "hello" then
            failwith ("test1 failed")
        if (strip_chars ("...world...") (".")) <> "world" then
            failwith ("test2 failed")
        if (strip_chars ("123hello123") ("123")) <> "hello" then
            failwith ("test3 failed")
        if (strip ("")) <> "" then
            failwith ("test4 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_strip()
        printfn "%s" (strip ("   hello   "))
        printfn "%s" (strip_chars ("...world...") ("."))
        printfn "%s" (strip_chars ("123hello123") ("123"))
        printfn "%s" (strip (""))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

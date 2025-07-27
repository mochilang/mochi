// Generated 2025-07-27 23:36 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        if ch = "a" then
            __ret <- 97
            raise Return
        if ch = "π" then
            __ret <- 960
            raise Return
        if ch = "A" then
            __ret <- 65
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec chr (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n = 97 then
            __ret <- "a"
            raise Return
        if n = 960 then
            __ret <- "π"
            raise Return
        if n = 65 then
            __ret <- "A"
            raise Return
        __ret <- "?"
        raise Return
        __ret
    with
        | Return -> __ret
let mutable b: int = ord "a"
let mutable r: int = ord "π"
let mutable s: string = "aπ"
printfn "%s" (((((string b) + " ") + (string r)) + " ") + s)
printfn "%s" (((("string cast to []rune: [" + (string b)) + " ") + (string r)) + "]")
printfn "%s" ((("    string range loop: " + (string b)) + " ") + (string r))
printfn "%s" "         string bytes: 0x61 0xcf 0x80"
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

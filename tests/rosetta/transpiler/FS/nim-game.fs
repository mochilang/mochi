// Generated 2025-08-02 10:37 +0700

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
let _dictAdd (d:System.Collections.Generic.IDictionary<string, obj>) (k:string) (v:obj) =
    d.[k] <- v
    d
let _dictCreate<'T> (pairs:(string * 'T) list) : System.Collections.Generic.IDictionary<string,'T> =
    let d = System.Collections.Generic.Dictionary<string, 'T>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
open System.Collections.Generic

open System

let rec parseIntStr (str: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable str = str
    try
        let mutable i: int = 0
        let mutable neg: bool = false
        if ((String.length (str)) > 0) && ((str.Substring(0, 1 - 0)) = "-") then
            neg <- true
            i <- 1
        let mutable n: int = 0
        let digits: System.Collections.Generic.IDictionary<string, int> = _dictCreate<int> [(string "0", 0); (string "1", 1); (string "2", 2); (string "3", 3); (string "4", 4); (string "5", 5); (string "6", 6); (string "7", 7); (string "8", 8); (string "9", 9)]
        while i < (String.length (str)) do
            n <- int ((n * 10) + (unbox<int> (digits.[(string (str.Substring(i, (i + 1) - i)))])))
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and showTokens (tokens: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable tokens = tokens
    try
        printfn "%s" ("Tokens remaining " + (string (tokens)))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable tokens: int = 12
        let mutable ``done``: bool = false
        while not ``done`` do
            showTokens (tokens)
            printfn "%s" ("")
            printfn "%s" ("How many tokens 1, 2 or 3?")
            let line: string = System.Console.ReadLine()
            let mutable t: int = 0
            if (String.length (line)) > 0 then
                t <- parseIntStr (line)
            if (t < 1) || (t > 3) then
                printfn "%s" ("\nMust be a number between 1 and 3, try again.\n")
            else
                let mutable ct: int = 4 - t
                let mutable s: string = "s"
                if ct = 1 then
                    s <- ""
                printfn "%s" (((("  Computer takes " + (string (ct))) + " token") + s) + "\n\n")
                tokens <- tokens - 4
            if tokens = 0 then
                showTokens (0)
                printfn "%s" ("  Computer wins!")
                ``done`` <- true
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

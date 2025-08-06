// Generated 2025-08-06 23:33 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec index_of (s: string) (c: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (string (s.[i])) = c then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and atbash (sequence: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable sequence = sequence
    try
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower_rev: string = "zyxwvutsrqponmlkjihgfedcba"
        let upper_rev: string = "ZYXWVUTSRQPONMLKJIHGFEDCBA"
        let mutable result: string = ""
        let mutable i: int = 0
        while i < (String.length (sequence)) do
            let ch: string = string (sequence.[i])
            let idx: int = index_of (lower) (ch)
            if idx <> (-1) then
                result <- result + (string (lower_rev.[idx]))
            else
                let idx2: int = index_of (upper) (ch)
                if idx2 <> (-1) then
                    result <- result + (string (upper_rev.[idx2]))
                else
                    result <- result + ch
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (atbash ("ABCDEFGH"))
printfn "%s" (atbash ("123GGjj"))
printfn "%s" (atbash ("testStringtest"))
printfn "%s" (atbash ("with space"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

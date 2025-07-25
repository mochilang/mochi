// Generated 2025-07-26 04:38 +0700

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
let rec char (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let letters: string = "abcdefghijklmnopqrstuvwxyz"
        let idx: int = n - 97
        if (idx < 0) || (idx >= (String.length letters)) then
            __ret <- "?"
            raise Return
        __ret <- letters.Substring(idx, (idx + 1) - idx)
        raise Return
        __ret
    with
        | Return -> __ret
let rec fromBytes (bs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable bs = bs
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < (int (Array.length bs)) do
            s <- s + (unbox<string> (char (int (bs.[i]))))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let mutable b: int array = [|98; 105; 110; 97; 114; 121|]
printfn "%s" (string b)
let mutable c: int array = b
printfn "%s" (string c)
printfn "%s" (string (b = c))
let mutable d: int array = [||]
let mutable i: int = 0
while i < (int (Array.length b)) do
    d <- Array.append d [|b.[i]|]
    i <- i + 1
d.[1] <- 97
d.[4] <- 110
printfn "%A" (fromBytes b)
printfn "%A" (fromBytes d)
printfn "%s" (string ((int (Array.length b)) = 0))
let mutable z = Array.append b [|122|]
printfn "%A" (fromBytes z)
let mutable sub = Array.sub b 1 (3 - 1)
printfn "%A" (fromBytes sub)
let mutable f: int array = [||]
i <- 0
while i < (int (Array.length d)) do
    let ``val``: int = d.[i]
    if ``val`` = 110 then
        f <- Array.append f [|109|]
    else
        f <- Array.append f [|``val``|]
    i <- i + 1
printfn "%s" (((unbox<string> (fromBytes d)) + " -> ") + (unbox<string> (fromBytes f)))
let mutable rem: int array = [||]
rem <- Array.append rem [|b.[0]|]
i <- 3
while i < (int (Array.length b)) do
    rem <- Array.append rem [|b.[i]|]
    i <- i + 1
printfn "%A" (fromBytes rem)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

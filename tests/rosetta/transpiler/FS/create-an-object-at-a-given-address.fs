// Generated 2025-07-30 21:41 +0700

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
let rec listStr (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length xs) do
            s <- s + (string (xs.[i]))
            if i < ((Seq.length xs) - 1) then
                s <- s + " "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec pointerDemo () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        printfn "%s" "Pointer:"
        let mutable i: int = 0
        printfn "%s" "Before:"
        printfn "%s" ((("\t<address>: " + (string i)) + ", ") + (string i))
        i <- 3
        printfn "%s" "After:"
        printfn "%s" ((("\t<address>: " + (string i)) + ", ") + (string i))
        __ret
    with
        | Return -> __ret
let rec sliceDemo () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        printfn "%s" "Slice:"
        let mutable a = [||]
        for _ in 0 .. (10 - 1) do
            a <- Array.append a [|0|]
        let mutable s = a
        printfn "%s" "Before:"
        printfn "%s" ("\ts: " + (unbox<string> (listStr s)))
        printfn "%s" ("\ta: " + (unbox<string> (listStr a)))
        let mutable data: int array = [|65; 32; 115; 116; 114; 105; 110; 103; 46|]
        let mutable idx: int = 0
        while idx < (Seq.length data) do
            s.[idx] <- data.[idx]
            idx <- idx + 1
        printfn "%s" "After:"
        printfn "%s" ("\ts: " + (unbox<string> (listStr s)))
        printfn "%s" ("\ta: " + (unbox<string> (listStr a)))
        __ret
    with
        | Return -> __ret
pointerDemo()
printfn "%s" ""
sliceDemo()
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

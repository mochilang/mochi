// Generated 2025-07-25 22:14 +0700

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
let rec concatInts (a: int array) (b: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        let mutable out: int array = [||]
        for v in a do
            out <- Array.append out [|v|]
        for v in b do
            out <- Array.append out [|v|]
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec concatAny (a: obj array) (b: obj array) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable a = a
    let mutable b = b
    try
        let mutable out: obj array = [||]
        for v in a do
            out <- Array.append out [|v|]
        for v in b do
            out <- Array.append out [|v|]
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let mutable a: int array = [|1; 2; 3|]
let mutable b: int array = [|7; 12; 60|]
printfn "%s" (string (concatInts a b))
let mutable i: obj array = [|1; 2; 3|]
let mutable j: obj array = [|"Crosby"; "Stills"; "Nash"; "Young"|]
printfn "%s" (string (concatAny i j))
let mutable l: int array = [|1; 2; 3|]
let mutable m: int array = [|7; 12; 60|]
printfn "%s" (string (concatInts l m))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

// Generated 2025-07-28 10:03 +0700

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
open System

module testpkg =
    let rec Add (a: int) (b: int) =
        let mutable __ret : int = Unchecked.defaultof<int>
        let mutable a = a
        let mutable b = b
        try
            __ret <- a + b
            raise Return
            __ret
        with
            | Return -> __ret
    let rec FifteenPuzzleExample () =
        let mutable __ret : string = Unchecked.defaultof<string>
        try
            __ret <- "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"
            raise Return
            __ret
        with
            | Return -> __ret
    let Pi = 3.14
    let Answer = 42

let res = testpkg.ECDSAExample()
printfn "%s" ("Private key:\nD: " + (unbox<string> (((res :?> Map<string, obj>).["D"]))))
printfn "%s" "\nPublic key:"
printfn "%s" ("X: " + (unbox<string> (((res :?> Map<string, obj>).["X"]))))
printfn "%s" ("Y: " + (unbox<string> (((res :?> Map<string, obj>).["Y"]))))
printfn "%s" "\nMessage: Rosetta Code"
printfn "%s" ("Hash   : " + (unbox<string> (((res :?> Map<string, obj>).["Hash"]))))
printfn "%s" "\nSignature:"
printfn "%s" ("R: " + (unbox<string> (((res :?> Map<string, obj>).["R"]))))
printfn "%s" ("S: " + (unbox<string> (((res :?> Map<string, obj>).["S"]))))
printfn "%s" ("\nSignature verified: " + (string (((res :?> Map<string, obj>).["Valid"]))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

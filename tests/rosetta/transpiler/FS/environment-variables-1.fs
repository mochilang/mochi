// Generated 2025-08-02 17:26 +0700

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

module os =
    let rec Getenv k =
        let mutable __ret : string = Unchecked.defaultof<string>
        let mutable k = k
        try
            __ret <- System.Environment.GetEnvironmentVariable (k)
            raise Return
            __ret
        with
            | Return -> __ret
    let rec Environ () =
        let mutable __ret : string array = Unchecked.defaultof<string array>
        try
            __ret <- Seq.toArray (Seq.map (            fun (de: System.Collections.DictionaryEntry) -> (sprintf "%s=%s" (string (de.Key)) (string (de.Value)))) (Seq.cast<System.Collections.DictionaryEntry> (System.Environment.GetEnvironmentVariables())))
            raise Return
            __ret
        with
            | Return -> __ret

printfn "%A" (os.Getenv("SHELL"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

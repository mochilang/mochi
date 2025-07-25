// Generated 2025-07-24 19:13 +0000

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
open System

let __memStart = System.GC.GetTotalMemory(false)
let __start = _now()
let n: int = 1000
let mutable s: int = 0
for i in 1 .. (n - 1) do
    s <- s + i
let __end = _now()
let __memEnd = System.GC.GetTotalMemory(false)
let __dur_us = (__end - __start) / 1000
let __mem_diff = __memEnd - __memStart
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"simple\"\n}" __dur_us __mem_diff

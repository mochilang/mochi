// Generated 2025-08-07 14:57 +0700

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

let rec binomial_coefficient (n: int) (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    let mutable k = k
    try
        let mutable result: int = 1
        let mutable kk: int = k
        if k > (n - k) then
            kk <- n - k
        for i in 0 .. (kk - 1) do
            result <- result * (n - i)
            result <- result / (i + 1)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec catalan_number (node_count: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable node_count = node_count
    try
        __ret <- (binomial_coefficient (2 * node_count) (node_count)) / (node_count + 1)
        raise Return
        __ret
    with
        | Return -> __ret
let rec factorial (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 0 then
            printfn "%s" ("factorial() not defined for negative values")
            __ret <- 0
            raise Return
        let mutable result: int = 1
        for i in 1 .. ((n + 1) - 1) do
            result <- result * i
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec binary_tree_count (node_count: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable node_count = node_count
    try
        __ret <- (catalan_number (node_count)) * (factorial (node_count))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" ("Enter the number of nodes:")
let input_str: string = System.Console.ReadLine()
let node_count: int = int (input_str)
if node_count <= 0 then
    printfn "%s" ("We need some nodes to work with.")
else
    let bst: int = catalan_number (node_count)
    let bt: int = binary_tree_count (node_count)
    printfn "%s" (String.concat " " ([|sprintf "%s" ("Given"); sprintf "%d" (node_count); sprintf "%s" ("nodes, there are"); sprintf "%d" (bt); sprintf "%s" ("binary trees and"); sprintf "%d" (bst); sprintf "%s" ("binary search trees.")|]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

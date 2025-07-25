// Generated 2025-07-26 04:38 +0700

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
let mutable arr1: int array = [|2; 7; 1; 8; 2|]
let mutable counts1: Map<int, int> = Map.ofList []
let mutable keys1: int array = [||]
let mutable i: int = 0
while i < (int (Array.length arr1)) do
    let v: int = arr1.[i]
    if Map.containsKey v counts1 then
        counts1 <- Map.add v ((int (counts1.[v] |> unbox<int>)) + 1) counts1
    else
        counts1 <- Map.add v 1 counts1
        keys1 <- Array.append keys1 [|v|]
    i <- i + 1
let mutable max1: int = 0
i <- 0
while i < (int (Array.length keys1)) do
    let k: int = keys1.[i]
    let c: int = counts1.[k] |> unbox<int>
    if c > max1 then
        max1 <- c
    i <- i + 1
let mutable modes1: int array = [||]
i <- 0
while i < (int (Array.length keys1)) do
    let k: int = keys1.[i]
    if (int (counts1.[k] |> unbox<int>)) = max1 then
        modes1 <- Array.append modes1 [|k|]
    i <- i + 1
printfn "%s" (string modes1)
let mutable arr2: int array = [|2; 7; 1; 8; 2; 8|]
let mutable counts2: Map<int, int> = Map.ofList []
let mutable keys2: int array = [||]
i <- 0
while i < (int (Array.length arr2)) do
    let v: int = arr2.[i]
    if Map.containsKey v counts2 then
        counts2 <- Map.add v ((int (counts2.[v] |> unbox<int>)) + 1) counts2
    else
        counts2 <- Map.add v 1 counts2
        keys2 <- Array.append keys2 [|v|]
    i <- i + 1
let mutable max2: int = 0
i <- 0
while i < (int (Array.length keys2)) do
    let k: int = keys2.[i]
    let c: int = counts2.[k] |> unbox<int>
    if c > max2 then
        max2 <- c
    i <- i + 1
let mutable modes2: int array = [||]
i <- 0
while i < (int (Array.length keys2)) do
    let k: int = keys2.[i]
    if (int (counts2.[k] |> unbox<int>)) = max2 then
        modes2 <- Array.append modes2 [|k|]
    i <- i + 1
printfn "%s" (string modes2)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

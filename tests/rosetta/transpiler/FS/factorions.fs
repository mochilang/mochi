// Generated 2025-08-04 20:44 +0700

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
let mutable facts: int array = [|1|]
let mutable n: int = 1
while n < 12 do
    facts <- Array.append facts [|(_idx facts (n - 1)) * n|]
    n <- n + 1
for b in 9 .. (13 - 1) do
    printfn "%s" (("The factorions for base " + (string (b))) + " are:")
    let mutable line: string = ""
    let mutable i: int = 1
    while i < 1500000 do
        let mutable m: int = i
        let mutable sum: int = 0
        while m > 0 do
            let d: int = ((m % b + b) % b)
            sum <- sum + (_idx facts (d))
            m <- m / b
        if sum = i then
            line <- (line + (string (i))) + " "
        i <- i + 1
    printfn "%s" (line)
    printfn "%s" ("")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

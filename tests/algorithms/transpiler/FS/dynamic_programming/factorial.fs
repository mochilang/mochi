// Generated 2025-08-07 15:46 +0700

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
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let mutable memo: int array = [|1; 1|]
let rec factorial (num: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    try
        if num < 0 then
            printfn "%s" ("Number should not be negative.")
            __ret <- 0
            raise Return
        let mutable m: int array = memo
        let mutable i: int = Seq.length (m)
        while i <= num do
            m <- Array.append m [|i * (_idx m (i - 1))|]
            i <- i + 1
        memo <- m
        __ret <- _idx m (num)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (factorial (7)))
factorial (-1)
let mutable results: int array = [||]
for i in 0 .. (10 - 1) do
    results <- Array.append results [|factorial (i)|]
printfn "%s" (_str (results))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

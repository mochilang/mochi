// Generated 2025-08-11 16:20 +0700

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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec max_tasks (tasks_info: int array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable tasks_info = tasks_info
    try
        let mutable order: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (tasks_info)) do
            order <- Array.append order [|i|]
            i <- i + 1
        let mutable n: int = Seq.length (order)
        i <- 0
        while i < n do
            let mutable j: int = i + 1
            while j < n do
                if (_idx (_idx tasks_info (int (_idx order (int j)))) (int 1)) > (_idx (_idx tasks_info (int (_idx order (int i)))) (int 1)) then
                    let tmp: int = _idx order (int i)
                    order.[int i] <- _idx order (int j)
                    order.[int j] <- tmp
                j <- j + 1
            i <- i + 1
        let mutable result: int array = Array.empty<int>
        let mutable pos: int = 1
        i <- 0
        while i < n do
            let id: int = _idx order (int i)
            let deadline: int = _idx (_idx tasks_info (int id)) (int 0)
            if deadline >= pos then
                result <- Array.append result [|id|]
            i <- i + 1
            pos <- pos + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let ex1: int array array = [|[|4; 20|]; [|1; 10|]; [|1; 40|]; [|1; 30|]|]
        let ex2: int array array = [|[|1; 10|]; [|2; 20|]; [|3; 30|]; [|2; 40|]|]
        printfn "%s" (_str (max_tasks (ex1)))
        printfn "%s" (_str (max_tasks (ex2)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

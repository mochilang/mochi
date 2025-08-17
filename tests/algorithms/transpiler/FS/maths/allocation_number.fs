// Generated 2025-08-17 08:49 +0700

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
let rec _str v =
    match box v with
    | :? float as f -> sprintf "%.15g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec allocation_num (number_of_bytes: int) (partitions: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable number_of_bytes = number_of_bytes
    let mutable partitions = partitions
    try
        if partitions <= 0 then
            ignore (failwith ("partitions must be a positive number!"))
        if partitions > number_of_bytes then
            ignore (failwith ("partitions can not > number_of_bytes!"))
        let bytes_per_partition: int = _floordiv (int number_of_bytes) (int partitions)
        let mutable allocation_list: string array = Array.empty<string>
        let mutable i: int = 0
        while i < partitions do
            let start_bytes: int = (i * bytes_per_partition) + 1
            let end_bytes: int = if i = (partitions - 1) then number_of_bytes else ((i + 1) * bytes_per_partition)
            allocation_list <- Array.append allocation_list [|(((_str (start_bytes)) + "-") + (_str (end_bytes)))|]
            i <- i + 1
        __ret <- allocation_list
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (allocation_num (16647) (4))))
ignore (printfn "%s" (_str (allocation_num (50000) (5))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

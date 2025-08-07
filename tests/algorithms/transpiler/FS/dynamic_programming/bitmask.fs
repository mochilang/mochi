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
let rec count_assignments (person: int) (task_performed: int array array) (used: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable person = person
    let mutable task_performed = task_performed
    let mutable used = used
    try
        if person = (Seq.length (task_performed)) then
            __ret <- 1
            raise Return
        let mutable total: int = 0
        let tasks: int array = _idx task_performed (person)
        let mutable i: int = 0
        while i < (Seq.length (tasks)) do
            let t: int = _idx tasks (i)
            if not (Seq.contains t used) then
                total <- total + (count_assignments (person + 1) (task_performed) (Array.append used [|t|]))
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and count_no_of_ways (task_performed: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable task_performed = task_performed
    try
        __ret <- count_assignments (0) (task_performed) (Array.empty<int>)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let task_performed: int array array = [|[|1; 3; 4|]; [|1; 2; 5|]; [|3; 4|]|]
        printfn "%s" (_str (count_no_of_ways (task_performed)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

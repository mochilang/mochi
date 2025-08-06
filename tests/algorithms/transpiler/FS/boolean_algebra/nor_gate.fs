// Generated 2025-08-06 22:14 +0700

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
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec nor_gate (input_1: int) (input_2: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable input_1 = input_1
    let mutable input_2 = input_2
    try
        __ret <- if (input_1 = 0) && (input_2 = 0) then 1 else 0
        raise Return
        __ret
    with
        | Return -> __ret
and center (s: string) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable width = width
    try
        let mutable total: int = width - (String.length (s))
        if total <= 0 then
            __ret <- s
            raise Return
        let mutable left: int = total / 2
        let mutable right: int = total - left
        let mutable res: string = s
        let mutable i: int = 0
        while i < left do
            res <- " " + res
            i <- i + 1
        let mutable j: int = 0
        while j < right do
            res <- res + " "
            j <- j + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and make_table_row (i: int) (j: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable i = i
    let mutable j = j
    try
        let output: int = nor_gate (i) (j)
        __ret <- ((((("| " + (center (_str (i)) (8))) + " | ") + (center (_str (j)) (8))) + " | ") + (center (_str (output)) (8))) + " |"
        raise Return
        __ret
    with
        | Return -> __ret
and truth_table () =
    let mutable __ret : string = Unchecked.defaultof<string>
    try
        __ret <- ((((((("Truth Table of NOR Gate:\n" + "| Input 1 | Input 2 | Output  |\n") + (make_table_row (0) (0))) + "\n") + (make_table_row (0) (1))) + "\n") + (make_table_row (1) (0))) + "\n") + (make_table_row (1) (1))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (nor_gate (0) (0))
printfn "%d" (nor_gate (0) (1))
printfn "%d" (nor_gate (1) (0))
printfn "%d" (nor_gate (1) (1))
printfn "%s" (truth_table())
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

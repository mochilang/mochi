// Generated 2025-08-06 21:33 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec and_gate (input_1: int) (input_2: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable input_1 = input_1
    let mutable input_2 = input_2
    try
        __ret <- if (input_1 <> 0) && (input_2 <> 0) then 1 else 0
        raise Return
        __ret
    with
        | Return -> __ret
and n_input_and_gate (inputs: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable inputs = inputs
    try
        let mutable i: int = 0
        while i < (Seq.length (inputs)) do
            if (_idx inputs (i)) = 0 then
                __ret <- 0
                raise Return
            i <- i + 1
        __ret <- 1
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (and_gate (0) (0))
printfn "%d" (and_gate (0) (1))
printfn "%d" (and_gate (1) (0))
printfn "%d" (and_gate (1) (1))
printfn "%d" (n_input_and_gate (unbox<int array> [|1; 0; 1; 1; 0|]))
printfn "%d" (n_input_and_gate (unbox<int array> [|1; 1; 1; 1; 1|]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

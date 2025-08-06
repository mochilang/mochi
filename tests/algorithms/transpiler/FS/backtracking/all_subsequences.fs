// Generated 2025-08-06 20:48 +0700

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec create_state_space_tree (sequence: obj array) (current: obj array) (index: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable sequence = sequence
    let mutable current = current
    let mutable index = index
    try
        if index = (Seq.length(sequence)) then
            printfn "%s" (_repr (current))
            __ret <- ()
            raise Return
        create_state_space_tree (sequence) (current) (index + 1)
        let with_elem: obj array = Array.append current [|unbox<obj> ((_idx sequence (index)))|]
        create_state_space_tree (sequence) (with_elem) (index + 1)
        __ret
    with
        | Return -> __ret
and generate_all_subsequences (sequence: obj array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable sequence = sequence
    try
        create_state_space_tree (sequence) ([||]) (0)
        __ret
    with
        | Return -> __ret
let seq: obj array = [|1; 2; 3|]
generate_all_subsequences (seq)
let seq2: obj array = [|"A"; "B"; "C"|]
generate_all_subsequences (seq2)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

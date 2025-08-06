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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec repeat_bool (times: int) =
    let mutable __ret : bool array = Unchecked.defaultof<bool array>
    let mutable times = times
    try
        let mutable res: bool array = [||]
        let mutable i: int = 0
        while i < times do
            res <- Array.append res [|false|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and set_bool (xs: bool array) (idx: int) (value: bool) =
    let mutable __ret : bool array = Unchecked.defaultof<bool array>
    let mutable xs = xs
    let mutable idx = idx
    let mutable value = value
    try
        let mutable res: bool array = [||]
        let mutable i: int = 0
        while i < (Seq.length(xs)) do
            if i = idx then
                res <- Array.append res [|value|]
            else
                res <- Array.append res [|_idx xs (i)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and create_state_space_tree (sequence: obj array) (current: obj array) (index: int) (used: bool array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable sequence = sequence
    let mutable current = current
    let mutable index = index
    let mutable used = used
    try
        if index = (Seq.length(sequence)) then
            printfn "%s" (_str (current))
            __ret <- ()
            raise Return
        let mutable i: int = 0
        while i < (Seq.length(sequence)) do
            if not (_idx used (i)) then
                let next_current: obj array = Array.append current [|unbox<obj> ((_idx sequence (i)))|]
                let next_used: bool array = set_bool (used) (i) (true)
                create_state_space_tree (sequence) (next_current) (index + 1) (next_used)
            i <- i + 1
        __ret
    with
        | Return -> __ret
and generate_all_permutations (sequence: obj array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable sequence = sequence
    try
        let used: bool array = repeat_bool (Seq.length(sequence))
        create_state_space_tree (sequence) ([||]) (0) (used)
        __ret
    with
        | Return -> __ret
let sequence: obj array = [|3; 1; 2; 4|]
generate_all_permutations (sequence)
let sequence_2: obj array = [|"A"; "B"; "C"|]
generate_all_permutations (sequence_2)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

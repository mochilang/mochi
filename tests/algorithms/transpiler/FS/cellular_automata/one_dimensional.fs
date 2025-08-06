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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec format_ruleset (ruleset: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable ruleset = ruleset
    try
        let mutable rs: int = ruleset
        let mutable bits_rev: int array = [||]
        let mutable i: int = 0
        while i < 8 do
            bits_rev <- Array.append bits_rev [|((rs % 2 + 2) % 2)|]
            rs <- rs / 2
            i <- i + 1
        let mutable bits: int array = [||]
        let mutable j: int = (Seq.length (bits_rev)) - 1
        while j >= 0 do
            bits <- Array.append bits [|_idx bits_rev (j)|]
            j <- j - 1
        __ret <- bits
        raise Return
        __ret
    with
        | Return -> __ret
and new_generation (cells: int array array) (rules: int array) (time: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable cells = cells
    let mutable rules = rules
    let mutable time = time
    try
        let population: int = Seq.length (_idx cells (0))
        let mutable next_generation: int array = [||]
        let mutable i: int = 0
        while i < population do
            let left_neighbor: int = if i = 0 then 0 else (_idx (_idx cells (time)) (i - 1))
            let right_neighbor: int = if i = (population - 1) then 0 else (_idx (_idx cells (time)) (i + 1))
            let center: int = _idx (_idx cells (time)) (i)
            let idx: int = 7 - (((left_neighbor * 4) + (center * 2)) + right_neighbor)
            next_generation <- Array.append next_generation [|_idx rules (idx)|]
            i <- i + 1
        __ret <- next_generation
        raise Return
        __ret
    with
        | Return -> __ret
and cells_to_string (row: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable row = row
    try
        let mutable result: string = ""
        let mutable i: int = 0
        while i < (Seq.length (row)) do
            if (_idx row (i)) = 1 then
                result <- result + "#"
            else
                result <- result + "."
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let initial: int array = [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
let mutable cells: int array array = [|initial|]
let rules: int array = format_ruleset (30)
let mutable time: int = 0
while time < 16 do
    let next: int array = new_generation (cells) (rules) (time)
    cells <- Array.append cells [|next|]
    time <- time + 1
let mutable t: int = 0
while t < (Seq.length (cells)) do
    printfn "%s" (cells_to_string (_idx cells (t)))
    t <- t + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let GLIDER: int array array = [|[|0; 1; 0; 0; 0; 0; 0; 0|]; [|0; 0; 1; 0; 0; 0; 0; 0|]; [|1; 1; 1; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0|]|]
let BLINKER: int array array = [|[|0; 1; 0|]; [|0; 1; 0|]; [|0; 1; 0|]|]
let rec new_generation (cells: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable cells = cells
    try
        let rows: int = Seq.length (cells)
        let cols: int = Seq.length (_idx cells (0))
        let mutable next: int array array = [||]
        let mutable i: int = 0
        while i < rows do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < cols do
                let mutable count: int = 0
                if (i > 0) && (j > 0) then
                    count <- count + (_idx (_idx cells (i - 1)) (j - 1))
                if i > 0 then
                    count <- count + (_idx (_idx cells (i - 1)) (j))
                if (i > 0) && (j < (cols - 1)) then
                    count <- count + (_idx (_idx cells (i - 1)) (j + 1))
                if j > 0 then
                    count <- count + (_idx (_idx cells (i)) (j - 1))
                if j < (cols - 1) then
                    count <- count + (_idx (_idx cells (i)) (j + 1))
                if (i < (rows - 1)) && (j > 0) then
                    count <- count + (_idx (_idx cells (i + 1)) (j - 1))
                if i < (rows - 1) then
                    count <- count + (_idx (_idx cells (i + 1)) (j))
                if (i < (rows - 1)) && (j < (cols - 1)) then
                    count <- count + (_idx (_idx cells (i + 1)) (j + 1))
                let alive: bool = (_idx (_idx cells (i)) (j)) = 1
                if ((alive && (count >= 2)) && (count <= 3)) || ((not alive) && (count = 3)) then
                    row <- Array.append row [|1|]
                else
                    row <- Array.append row [|0|]
                j <- j + 1
            next <- Array.append next [|row|]
            i <- i + 1
        __ret <- next
        raise Return
        __ret
    with
        | Return -> __ret
and generate_generations (cells: int array array) (frames: int) =
    let mutable __ret : int array array array = Unchecked.defaultof<int array array array>
    let mutable cells = cells
    let mutable frames = frames
    try
        let mutable result: int array array array = [||]
        let mutable i: int = 0
        let mutable current: int array array = cells
        while i < frames do
            result <- Array.append result [|current|]
            current <- new_generation (current)
            i <- i + 1
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
        let frames: int array array array = generate_generations (GLIDER) (4)
        let mutable i: int = 0
        while i < (Seq.length (frames)) do
            printfn "%s" (_repr (_idx frames (i)))
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

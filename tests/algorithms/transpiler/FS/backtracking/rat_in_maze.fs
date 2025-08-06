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
let rec run_maze (maze: int array array) (i: int) (j: int) (dr: int) (dc: int) (sol: int array array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable maze = maze
    let mutable i = i
    let mutable j = j
    let mutable dr = dr
    let mutable dc = dc
    let mutable sol = sol
    try
        let size: int = Seq.length(maze)
        if ((i = dr) && (j = dc)) && ((_idx (_idx maze (i)) (j)) = 0) then
            sol.[i].[j] <- 0
            __ret <- true
            raise Return
        let lower_flag: bool = (i >= 0) && (j >= 0)
        let upper_flag: bool = (i < size) && (j < size)
        if lower_flag && upper_flag then
            let block_flag: bool = ((_idx (_idx sol (i)) (j)) = 1) && ((_idx (_idx maze (i)) (j)) = 0)
            if block_flag then
                sol.[i].[j] <- 0
                if (((run_maze (maze) (i + 1) (j) (dr) (dc) (sol)) || (run_maze (maze) (i) (j + 1) (dr) (dc) (sol))) || (run_maze (maze) (i - 1) (j) (dr) (dc) (sol))) || (run_maze (maze) (i) (j - 1) (dr) (dc) (sol)) then
                    __ret <- true
                    raise Return
                sol.[i].[j] <- 1
                __ret <- false
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and solve_maze (maze: int array array) (sr: int) (sc: int) (dr: int) (dc: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable maze = maze
    let mutable sr = sr
    let mutable sc = sc
    let mutable dr = dr
    let mutable dc = dc
    try
        let size: int = Seq.length(maze)
        if not ((((((((0 <= sr) && (sr < size)) && (0 <= sc)) && (sc < size)) && (0 <= dr)) && (dr < size)) && (0 <= dc)) && (dc < size)) then
            failwith ("Invalid source or destination coordinates")
        let mutable sol: int array array = [||]
        let mutable i: int = 0
        while i < size do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < size do
                row <- Array.append row [|1|]
                j <- j + 1
            sol <- Array.append sol [|row|]
            i <- i + 1
        let solved: bool = run_maze (maze) (sr) (sc) (dr) (dc) (sol)
        if solved then
            __ret <- sol
            raise Return
        else
            failwith ("No solution exists!")
        __ret
    with
        | Return -> __ret
let maze: int array array = [|[|0; 1; 0; 1; 1|]; [|0; 0; 0; 0; 0|]; [|1; 0; 1; 0; 1|]; [|0; 0; 1; 0; 0|]; [|1; 0; 0; 1; 0|]|]
let n: int = (Seq.length(maze)) - 1
printfn "%s" (_str (solve_maze (maze) (0) (0) (n) (n)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

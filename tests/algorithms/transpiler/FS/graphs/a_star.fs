// Generated 2025-08-13 16:01 +0700

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
type SearchResult = {
    mutable path: int array array
    mutable action: int array array
}
let DIRECTIONS: int array array = [|[|-1; 0|]; [|0; -1|]; [|1; 0|]; [|0; 1|]|]
let rec iabs (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- if x < 0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and search (grid: int array array) (init: int array) (goal: int array) (cost: int) (heuristic: int array array) =
    let mutable __ret : SearchResult = Unchecked.defaultof<SearchResult>
    let mutable grid = grid
    let mutable init = init
    let mutable goal = goal
    let mutable cost = cost
    let mutable heuristic = heuristic
    try
        let mutable closed: int array array = Array.empty<int array>
        let mutable r: int = 0
        while r < (Seq.length (grid)) do
            let mutable row: int array = Array.empty<int>
            let mutable c: int = 0
            while c < (Seq.length (_idx grid (int 0))) do
                row <- Array.append row [|0|]
                c <- c + 1
            closed <- Array.append closed [|row|]
            r <- r + 1
        closed.[(_idx init (int 0))].[(_idx init (int 1))] <- 1
        let mutable action: int array array = Array.empty<int array>
        r <- 0
        while r < (Seq.length (grid)) do
            let mutable row: int array = Array.empty<int>
            let mutable c: int = 0
            while c < (Seq.length (_idx grid (int 0))) do
                row <- Array.append row [|0|]
                c <- c + 1
            action <- Array.append action [|row|]
            r <- r + 1
        let mutable x: int = _idx init (int 0)
        let mutable y: int = _idx init (int 1)
        let mutable g: int = 0
        let mutable f: int = g + (_idx (_idx heuristic (int x)) (int y))
        let mutable cell: int array array = [|[|f; g; x; y|]|]
        let mutable found: bool = false
        let mutable resign: bool = false
        while (not found) && (not resign) do
            if (Seq.length (cell)) = 0 then
                ignore (failwith ("Algorithm is unable to find solution"))
            else
                let mutable best_i: int = 0
                let mutable best_f: int = _idx (_idx cell (int 0)) (int 0)
                let mutable i: int = 1
                while i < (Seq.length (cell)) do
                    if (_idx (_idx cell (int i)) (int 0)) < best_f then
                        best_f <- _idx (_idx cell (int i)) (int 0)
                        best_i <- i
                    i <- i + 1
                let next_cell: int array = _idx cell (int best_i)
                let mutable new_cell: int array array = Array.empty<int array>
                i <- 0
                while i < (Seq.length (cell)) do
                    if i <> best_i then
                        new_cell <- Array.append new_cell [|(_idx cell (int i))|]
                    i <- i + 1
                cell <- new_cell
                x <- _idx next_cell (int 2)
                y <- _idx next_cell (int 3)
                g <- _idx next_cell (int 1)
                if (x = (_idx goal (int 0))) && (y = (_idx goal (int 1))) then
                    found <- true
                else
                    let mutable d: int = 0
                    while d < (Seq.length (DIRECTIONS)) do
                        let x2: int = x + (_idx (_idx DIRECTIONS (int d)) (int 0))
                        let y2: int = y + (_idx (_idx DIRECTIONS (int d)) (int 1))
                        if (((((x2 >= 0) && (x2 < (Seq.length (grid)))) && (y2 >= 0)) && (y2 < (Seq.length (_idx grid (int 0))))) && ((_idx (_idx closed (int x2)) (int y2)) = 0)) && ((_idx (_idx grid (int x2)) (int y2)) = 0) then
                            let g2: int = g + cost
                            let f2: int = g2 + (_idx (_idx heuristic (int x2)) (int y2))
                            cell <- Array.append cell [|[|f2; g2; x2; y2|]|]
                            closed.[x2].[y2] <- 1
                            action.[x2].[y2] <- d
                        d <- d + 1
        let mutable invpath: int array array = Array.empty<int array>
        x <- _idx goal (int 0)
        y <- _idx goal (int 1)
        invpath <- Array.append invpath [|[|x; y|]|]
        while (x <> (_idx init (int 0))) || (y <> (_idx init (int 1))) do
            let dir: int = _idx (_idx action (int x)) (int y)
            let x2: int = x - (_idx (_idx DIRECTIONS (int dir)) (int 0))
            let y2: int = y - (_idx (_idx DIRECTIONS (int dir)) (int 1))
            x <- x2
            y <- y2
            invpath <- Array.append invpath [|[|x; y|]|]
        let mutable path: int array array = Array.empty<int array>
        let mutable idx: int = (Seq.length (invpath)) - 1
        while idx >= 0 do
            path <- Array.append path [|(_idx invpath (int idx))|]
            idx <- idx - 1
        __ret <- { path = path; action = action }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let grid: int array array = [|[|0; 1; 0; 0; 0; 0|]; [|0; 1; 0; 0; 0; 0|]; [|0; 1; 0; 0; 0; 0|]; [|0; 1; 0; 0; 1; 0|]; [|0; 0; 0; 0; 1; 0|]|]
        let init: int array = unbox<int array> [|0; 0|]
        let goal: int array = unbox<int array> [|(Seq.length (grid)) - 1; (Seq.length (_idx grid (int 0))) - 1|]
        let cost: int = 1
        let mutable heuristic: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < (Seq.length (grid)) do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < (Seq.length (_idx grid (int 0))) do
                let h: int = (iabs (i - (_idx goal (int 0)))) + (iabs (j - (_idx goal (int 1))))
                if (_idx (_idx grid (int i)) (int j)) = 1 then
                    row <- Array.append row [|99|]
                else
                    row <- Array.append row [|h|]
                j <- j + 1
            heuristic <- Array.append heuristic [|row|]
            i <- i + 1
        let result: SearchResult = search (grid) (init) (goal) (cost) (heuristic)
        ignore (printfn "%s" ("ACTION MAP"))
        let mutable rr: int = 0
        while rr < (Seq.length (result.action)) do
            ignore (printfn "%s" (_repr (_idx (result.action) (int rr))))
            rr <- rr + 1
        let mutable p: int = 0
        while p < (Seq.length (result.path)) do
            ignore (printfn "%s" (_repr (_idx (result.path) (int p))))
            p <- p + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

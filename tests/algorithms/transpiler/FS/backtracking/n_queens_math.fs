// Generated 2025-08-06 21:04 +0700

exception Break
exception Continue

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
let rec contains (xs: int array) (x: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) = x then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and repeat (s: string) (times: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable times = times
    try
        let mutable result: string = ""
        let mutable i: int = 0
        while i < times do
            result <- result + s
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and build_board (pos: int array) (n: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable pos = pos
    let mutable n = n
    try
        let mutable board: string array = [||]
        let mutable i: int = 0
        while i < (Seq.length (pos)) do
            let mutable col: int = _idx pos (i)
            let line: string = ((repeat (". ") (col)) + "Q ") + (repeat (". ") ((n - 1) - col))
            board <- Array.append board [|line|]
            i <- i + 1
        __ret <- board
        raise Return
        __ret
    with
        | Return -> __ret
and depth_first_search (pos: int array) (dr: int array) (dl: int array) (n: int) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable pos = pos
    let mutable dr = dr
    let mutable dl = dl
    let mutable n = n
    try
        let row: int = Seq.length (pos)
        if row = n then
            let mutable single: string array array = [||]
            single <- Array.append single [|build_board (pos) (n)|]
            __ret <- single
            raise Return
        let mutable boards: string array array = [||]
        let mutable col: int = 0
        try
            while col < n do
                try
                    if ((contains (pos) (col)) || (contains (dr) (row - col))) || (contains (dl) (row + col)) then
                        col <- col + 1
                        raise Continue
                    let mutable result: string array array = depth_first_search (Array.append pos [|col|]) (Array.append dr [|row - col|]) (Array.append dl [|row + col|]) (n)
                    boards <- unbox<string array array> (Array.append (boards) (result))
                    col <- col + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- boards
        raise Return
        __ret
    with
        | Return -> __ret
and n_queens_solution (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable boards: string array array = depth_first_search (Array.empty<int>) (Array.empty<int>) (Array.empty<int>) (n)
        let mutable i: int = 0
        while i < (Seq.length (boards)) do
            let mutable j: int = 0
            while j < (Seq.length (_idx boards (i))) do
                printfn "%s" (_idx (_idx boards (i)) (j))
                j <- j + 1
            printfn "%s" ("")
            i <- i + 1
        printfn "%s" (String.concat " " ([|sprintf "%d" (Seq.length (boards)); sprintf "%s" ("solutions were found.")|]))
        __ret <- Seq.length (boards)
        raise Return
        __ret
    with
        | Return -> __ret
n_queens_solution (4)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

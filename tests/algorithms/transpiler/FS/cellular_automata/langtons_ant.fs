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
let rec create_board (width: int) (height: int) =
    let mutable __ret : bool array array = Unchecked.defaultof<bool array array>
    let mutable width = width
    let mutable height = height
    try
        let mutable board: bool array array = [||]
        let mutable i: int = 0
        while i < height do
            let mutable row: bool array = [||]
            let mutable j: int = 0
            while j < width do
                row <- Array.append row [|true|]
                j <- j + 1
            board <- Array.append board [|row|]
            i <- i + 1
        __ret <- board
        raise Return
        __ret
    with
        | Return -> __ret
and move_ant (board: bool array array) (x: int) (y: int) (direction: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable board = board
    let mutable x = x
    let mutable y = y
    let mutable direction = direction
    try
        if _idx (_idx board (x)) (y) then
            direction <- (((direction + 1) % 4 + 4) % 4)
        else
            direction <- (((direction + 3) % 4 + 4) % 4)
        let old_x: int = x
        let old_y: int = y
        if direction = 0 then
            x <- x - 1
        else
            if direction = 1 then
                y <- y + 1
            else
                if direction = 2 then
                    x <- x + 1
                else
                    y <- y - 1
        board.[old_x].[old_y] <- not (_idx (_idx board (old_x)) (old_y))
        __ret <- unbox<int array> [|x; y; direction|]
        raise Return
        __ret
    with
        | Return -> __ret
and langtons_ant (width: int) (height: int) (steps: int) =
    let mutable __ret : bool array array = Unchecked.defaultof<bool array array>
    let mutable width = width
    let mutable height = height
    let mutable steps = steps
    try
        let mutable board: bool array array = create_board (width) (height)
        let mutable x: int = width / 2
        let mutable y: int = height / 2
        let mutable dir: int = 3
        let mutable s: int = 0
        while s < steps do
            let state: int array = move_ant (board) (x) (y) (dir)
            x <- _idx state (0)
            y <- _idx state (1)
            dir <- _idx state (2)
            s <- s + 1
        __ret <- board
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

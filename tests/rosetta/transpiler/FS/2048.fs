// Generated 2025-07-25 12:29 +0700

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
type Board = {
    cells: int array array
}
type SpawnResult = {
    board: Board
    full: bool
}
type SlideResult = {
    row: int array
    gain: int
}
type MoveResult = {
    board: Board
    score: int
    moved: bool
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System

let SIZE: int = 4
let rec newBoard () =
    let mutable __ret : Board = Unchecked.defaultof<Board>
    try
        let mutable b: int array array = [||]
        let mutable y: int = 0
        while y < SIZE do
            let mutable row: int array = [||]
            let mutable x: int = 0
            while x < SIZE do
                row <- Array.append row [|0|]
                x <- x + 1
            b <- Array.append b [|row|]
            y <- y + 1
        __ret <- { cells = b }
        raise Return
        __ret
    with
        | Return -> __ret
let rec spawnTile (b: Board) =
    let mutable __ret : SpawnResult = Unchecked.defaultof<SpawnResult>
    let mutable b = b
    try
        let mutable grid: int array array = b.cells
        let mutable empty: int array array = [||]
        let mutable y: int = 0
        while y < SIZE do
            let mutable x: int = 0
            while x < SIZE do
                if (grid.[y].[x]) = 0 then
                    empty <- Array.append empty [|[|x; y|]|]
                x <- x + 1
            y <- y + 1
        if (Array.length empty) = 0 then
            __ret <- { board = b; full = true }
            raise Return
        let mutable idx = (_now()) % (Array.length empty)
        let cell = empty.[idx]
        let mutable ``val``: int = 4
        if ((_now()) % 10) < 9 then
            ``val`` <- 2
        grid.[cell.[1]].[cell.[0]] <- ``val``
        __ret <- { board = { cells = grid }; full = (Array.length empty) = 1 }
        raise Return
        __ret
    with
        | Return -> __ret
let rec pad (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string n
        let mutable pad: int = 4 - (String.length s)
        let mutable i: int = 0
        let mutable out: string = ""
        while i < pad do
            out <- out + " "
            i <- i + 1
        __ret <- out + s
        raise Return
        __ret
    with
        | Return -> __ret
let rec draw (b: Board) (score: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable score = score
    try
        printfn "%s" ("Score: " + (string score))
        let mutable y: int = 0
        while y < SIZE do
            printfn "%s" "+----+----+----+----+"
            let mutable line: string = "|"
            let mutable x: int = 0
            while x < SIZE do
                let mutable v = b.cells.[y].[x]
                if v = 0 then
                    line <- line + "    |"
                else
                    line <- (line + (pad v)) + "|"
                x <- x + 1
            printfn "%s" line
            y <- y + 1
        printfn "%s" "+----+----+----+----+"
        printfn "%s" "W=Up S=Down A=Left D=Right Q=Quit"
        __ret
    with
        | Return -> __ret
let rec reverseRow (r: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable r = r
    try
        let mutable out: int array = [||]
        let mutable i = (Array.length r) - 1
        while i >= 0 do
            out <- Array.append out [|r.[i]|]
            i <- i - 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec slideLeft (row: int array) =
    let mutable __ret : SlideResult = Unchecked.defaultof<SlideResult>
    let mutable row = row
    try
        let mutable xs: int array = [||]
        let mutable i: int = 0
        while i < (Array.length row) do
            if (row.[i]) <> 0 then
                xs <- Array.append xs [|row.[i]|]
            i <- i + 1
        let mutable res: int array = [||]
        let mutable gain: int = 0
        i <- 0
        while i < (Array.length xs) do
            if ((i + 1) < (Array.length xs)) && ((xs.[i]) = (xs.[i + 1])) then
                let v = (xs.[i]) * 2
                gain <- gain + v
                res <- Array.append res [|v|]
                i <- i + 2
            else
                res <- Array.append res [|xs.[i]|]
                i <- i + 1
        while (Array.length res) < SIZE do
            res <- Array.append res [|0|]
        __ret <- { row = res; gain = gain }
        raise Return
        __ret
    with
        | Return -> __ret
let rec moveLeft (b: Board) (score: int) =
    let mutable __ret : MoveResult = Unchecked.defaultof<MoveResult>
    let mutable b = b
    let mutable score = score
    try
        let mutable grid: int array array = b.cells
        let mutable moved: bool = false
        let mutable y: int = 0
        while y < SIZE do
            let r = slideLeft (grid.[y])
            let ``new`` = r.row
            score <- score + (r.gain)
            let mutable x: int = 0
            while x < SIZE do
                if (grid.[y].[x]) <> (``new``.[x]) then
                    moved <- true
                grid.[y].[x] <- ``new``.[x]
                x <- x + 1
            y <- y + 1
        __ret <- { board = { cells = grid }; score = score; moved = moved }
        raise Return
        __ret
    with
        | Return -> __ret
let rec moveRight (b: Board) (score: int) =
    let mutable __ret : MoveResult = Unchecked.defaultof<MoveResult>
    let mutable b = b
    let mutable score = score
    try
        let mutable grid: int array array = b.cells
        let mutable moved: bool = false
        let mutable y: int = 0
        while y < SIZE do
            let mutable rev = reverseRow (grid.[y])
            let r = slideLeft rev
            rev <- r.row
            score <- score + (r.gain)
            rev <- reverseRow rev
            let mutable x: int = 0
            while x < SIZE do
                if (grid.[y].[x]) <> (rev.[x]) then
                    moved <- true
                grid.[y].[x] <- rev.[x]
                x <- x + 1
            y <- y + 1
        __ret <- { board = { cells = grid }; score = score; moved = moved }
        raise Return
        __ret
    with
        | Return -> __ret
let rec getCol (b: Board) (x: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable b = b
    let mutable x = x
    try
        let mutable col: int array = [||]
        let mutable y: int = 0
        while y < SIZE do
            col <- Array.append col [|b.cells.[y].[x]|]
            y <- y + 1
        __ret <- col
        raise Return
        __ret
    with
        | Return -> __ret
let rec setCol (b: Board) (x: int) (col: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable x = x
    let mutable col = col
    try
        let mutable rows: int array array = b.cells
        let mutable y: int = 0
        while y < SIZE do
            let mutable row = rows.[y]
            row.[x] <- col.[y]
            rows.[y] <- row
            y <- y + 1
        b <- { b with cells = rows }
        __ret
    with
        | Return -> __ret
let rec moveUp (b: Board) (score: int) =
    let mutable __ret : MoveResult = Unchecked.defaultof<MoveResult>
    let mutable b = b
    let mutable score = score
    try
        let mutable grid: int array array = b.cells
        let mutable moved: bool = false
        let mutable x: int = 0
        while x < SIZE do
            let mutable col = getCol b x
            let r = slideLeft col
            let ``new`` = r.row
            score <- score + (r.gain)
            let mutable y: int = 0
            while y < SIZE do
                if (grid.[y].[x]) <> (``new``.[y]) then
                    moved <- true
                grid.[y].[x] <- ``new``.[y]
                y <- y + 1
            x <- x + 1
        __ret <- { board = { cells = grid }; score = score; moved = moved }
        raise Return
        __ret
    with
        | Return -> __ret
let rec moveDown (b: Board) (score: int) =
    let mutable __ret : MoveResult = Unchecked.defaultof<MoveResult>
    let mutable b = b
    let mutable score = score
    try
        let mutable grid: int array array = b.cells
        let mutable moved: bool = false
        let mutable x: int = 0
        while x < SIZE do
            let mutable col = reverseRow (getCol b x)
            let r = slideLeft col
            col <- r.row
            score <- score + (r.gain)
            col <- reverseRow col
            let mutable y: int = 0
            while y < SIZE do
                if (grid.[y].[x]) <> (col.[y]) then
                    moved <- true
                grid.[y].[x] <- col.[y]
                y <- y + 1
            x <- x + 1
        __ret <- { board = { cells = grid }; score = score; moved = moved }
        raise Return
        __ret
    with
        | Return -> __ret
let rec hasMoves (b: Board) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable b = b
    try
        let mutable y: int = 0
        while y < SIZE do
            let mutable x: int = 0
            while x < SIZE do
                if (b.cells.[y].[x]) = 0 then
                    __ret <- true
                    raise Return
                if ((x + 1) < SIZE) && ((b.cells.[y].[x]) = (b.cells.[y].[x + 1])) then
                    __ret <- true
                    raise Return
                if ((y + 1) < SIZE) && ((b.cells.[y].[x]) = (b.cells.[y + 1].[x])) then
                    __ret <- true
                    raise Return
                x <- x + 1
            y <- y + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec has2048 (b: Board) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable b = b
    try
        let mutable y: int = 0
        while y < SIZE do
            let mutable x: int = 0
            while x < SIZE do
                if (b.cells.[y].[x]) >= 2048 then
                    __ret <- true
                    raise Return
                x <- x + 1
            y <- y + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let mutable board: Board = newBoard()
let mutable r = spawnTile board
board <- r.board
let mutable full = r.full
r <- spawnTile board
board <- r.board
full <- r.full
let mutable score: int = 0
draw board score
try
    while true do
        printfn "%s" "Move: "
        let cmd: string = System.Console.ReadLine()
        let mutable moved: bool = false
        if (cmd = "a") || (cmd = "A") then
            let m = moveLeft board score
            board <- m.board
            score <- m.score
            moved <- m.moved
        if (cmd = "d") || (cmd = "D") then
            let m = moveRight board score
            board <- m.board
            score <- m.score
            moved <- m.moved
        if (cmd = "w") || (cmd = "W") then
            let m = moveUp board score
            board <- m.board
            score <- m.score
            moved <- m.moved
        if (cmd = "s") || (cmd = "S") then
            let m = moveDown board score
            board <- m.board
            score <- m.score
            moved <- m.moved
        if (cmd = "q") || (cmd = "Q") then
            raise Break
        if moved then
            let r2 = spawnTile board
            board <- r2.board
            full <- r2.full
            if full && (not (hasMoves board)) then
                draw board score
                printfn "%s" "Game Over"
                raise Break
        draw board score
        if has2048 board then
            printfn "%s" "You win!"
            raise Break
        if not (hasMoves board) then
            printfn "%s" "Game Over"
            raise Break
with
| Break -> ()
| Continue -> ()
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

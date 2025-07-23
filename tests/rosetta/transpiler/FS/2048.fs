// Generated 2025-07-24 00:44 +0700

exception Break
exception Continue

let mutable _nowSeed = 0
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525 + 1013904223) % 2147483647
        _nowSeed
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
open System

let SIZE: int = 4
let rec newBoard () =
    let mutable b = [||]
    let mutable y: int = 0
    while y < SIZE do
        let mutable row = [||]
        let mutable x: int = 0
        while x < SIZE do
            row <- Array.append row [|0|]
            x <- x + 1
        b <- Array.append b [|row|]
        y <- y + 1
    { cells = b }
let rec spawnTile (b: Board) =
    let mutable grid: int array array = b.cells
    let mutable empty = [||]
    let mutable y: int = 0
    while y < SIZE do
        let mutable x: int = 0
        while x < SIZE do
            if (grid.[y].[x]) = 0 then
                empty <- Array.append empty [|[|x; y|]|]
            x <- x + 1
        y <- y + 1
    if (Array.length empty) = 0 then
        { board = b; full = true }
    let mutable idx = (_now()) % (Array.length empty)
    let cell = empty.[idx]
    let mutable ``val``: int = 4
    if ((_now()) % 10) < 9 then
        ``val`` <- 2
    grid.[cell.[1]].[cell.[0]] <- ``val``
    { board = { cells = grid }; full = (Array.length empty) = 1 }
let rec pad (n: int) =
    let mutable s: string = string n
    let mutable pad: int = 4 - (String.length s)
    let mutable i: int = 0
    let mutable out: string = ""
    while i < pad do
        out <- out + " "
        i <- i + 1
    out + s
let rec draw (b: Board) (score: int) =
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
let rec reverseRow (r: SpawnResult) =
    let mutable out = [||]
    let mutable i: int = (Seq.length r) - 1
    while i >= 0 do
        out <- Array.append out [|r.[i]|]
        i <- i - 1
    out
let rec slideLeft row =
    let mutable xs = [||]
    let mutable i: int = 0
    while i < (Seq.length row) do
        if (row.[i]) <> 0 then
            xs <- Array.append xs [|row.[i]|]
        i <- i + 1
    let mutable res = [||]
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
    { row = res; gain = gain }
let rec moveLeft (b: Board) score =
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
    { board = { cells = grid }; score = score; moved = moved }
let rec moveRight (b: Board) score =
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
    { board = { cells = grid }; score = score; moved = moved }
let rec getCol (b: Board) (x: int) =
    let mutable col = [||]
    let mutable y: int = 0
    while y < SIZE do
        col <- Array.append col [|b.cells.[y].[x]|]
        y <- y + 1
    col
let rec setCol (b: Board) (x: int) col =
    let mutable rows: int array array = b.cells
    let mutable y: int = 0
    while y < SIZE do
        let mutable row = rows.[y]
        row.[x] <- col.[y]
        rows.[y] <- row
        y <- y + 1
    { b with cells = rows }
let rec moveUp (b: Board) score =
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
    { board = { cells = grid }; score = score; moved = moved }
let rec moveDown (b: Board) score =
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
    { board = { cells = grid }; score = score; moved = moved }
let rec hasMoves (b: Board) =
    let mutable y: int = 0
    while y < SIZE do
        let mutable x: int = 0
        while x < SIZE do
            if (b.cells.[y].[x]) = 0 then
                true
            if ((x + 1) < SIZE) && ((b.cells.[y].[x]) = (b.cells.[y].[x + 1])) then
                true
            if ((y + 1) < SIZE) && ((b.cells.[y].[x]) = (b.cells.[y + 1].[x])) then
                true
            x <- x + 1
        y <- y + 1
    false
let rec has2048 (b: Board) =
    let mutable y: int = 0
    while y < SIZE do
        let mutable x: int = 0
        while x < SIZE do
            if (b.cells.[y].[x]) >= 2048 then
                true
            x <- x + 1
        y <- y + 1
    false
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
        let cmd = input()
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

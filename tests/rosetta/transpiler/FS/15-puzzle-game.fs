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
type MoveResult = {
    idx: int
    ok: bool
}
open System

let mutable board: int array = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 0|]
let solved: int array = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 0|]
let mutable empty: int = 15
let mutable moves: int = 0
let mutable quit: bool = false
let rec randMove () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        __ret <- (_now()) % 4
        raise Return
        __ret
    with
        | Return -> __ret
and isSolved () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let mutable i: int = 0
        while i < 16 do
            if (board.[i]) <> (solved.[i]) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and isValidMove (m: int) =
    let mutable __ret : MoveResult = Unchecked.defaultof<MoveResult>
    let mutable m = m
    try
        if m = 0 then
            __ret <- { idx = empty - 4; ok = (empty / 4) > 0 }
            raise Return
        if m = 1 then
            __ret <- { idx = empty + 4; ok = (empty / 4) < 3 }
            raise Return
        if m = 2 then
            __ret <- { idx = empty + 1; ok = (empty % 4) < 3 }
            raise Return
        if m = 3 then
            __ret <- { idx = empty - 1; ok = (empty % 4) > 0 }
            raise Return
        __ret <- { idx = 0; ok = false }
        raise Return
        __ret
    with
        | Return -> __ret
and doMove (m: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable m = m
    try
        let r = isValidMove m
        if not (r.ok) then
            __ret <- false
            raise Return
        let i: int = empty
        let j = r.idx
        let tmp = board.[i]
        board.[i] <- board.[j]
        board.[j] <- tmp
        empty <- j
        moves <- moves + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and shuffle (n: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    try
        let mutable i: int = 0
        while (i < n) || (isSolved()) do
            if doMove (randMove()) then
                i <- i + 1
        __ret
    with
        | Return -> __ret
and printBoard () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable line: string = ""
        let mutable i: int = 0
        while i < 16 do
            let ``val`` = board.[i]
            if ``val`` = 0 then
                line <- line + "  ."
            else
                let s: string = string ``val``
                if ``val`` < 10 then
                    line <- (line + "  ") + s
                else
                    line <- (line + " ") + s
            if (i % 4) = 3 then
                printfn "%s" line
                line <- ""
            i <- i + 1
        __ret
    with
        | Return -> __ret
and playOneMove () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        try
            while true do
                printfn "%s" (("Enter move #" + (string (moves + 1))) + " (U, D, L, R, or Q): ")
                let s: string = System.Console.ReadLine()
                if s = "" then
                    raise Continue
                let c: string = s.Substring(0, 1 - 0)
                let mutable m: int = 0
                if (c = "U") || (c = "u") then
                    m <- 0
                else
                    if (c = "D") || (c = "d") then
                        m <- 1
                    else
                        if (c = "R") || (c = "r") then
                            m <- 2
                        else
                            if (c = "L") || (c = "l") then
                                m <- 3
                            else
                                if (c = "Q") || (c = "q") then
                                    printfn "%s" (("Quiting after " + (string moves)) + " moves.")
                                    quit <- true
                                    __ret <- ()
                                    raise Return
                                else
                                    printfn "%s" ((("Please enter \"U\", \"D\", \"L\", or \"R\" to move the empty cell\n" + "up, down, left, or right. You can also enter \"Q\" to quit.\n") + "Upper or lowercase is accepted and only the first non-blank\n") + "character is important (i.e. you may enter \"up\" if you like).")
                                    raise Continue
                if not (doMove m) then
                    printfn "%s" "That is not a valid move at the moment."
                    raise Continue
                __ret <- ()
                raise Return
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
and play () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        printfn "%s" "Starting board:"
        while (not quit) && ((isSolved()) = false) do
            printfn "%s" ""
            printBoard()
            playOneMove()
        if isSolved() then
            printfn "%s" (("You solved the puzzle in " + (string moves)) + " moves.")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        shuffle 50
        play()
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

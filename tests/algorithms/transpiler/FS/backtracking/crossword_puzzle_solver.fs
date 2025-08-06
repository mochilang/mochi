// Generated 2025-08-06 16:21 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec is_valid (puzzle: string array array) (word: string) (row: int) (col: int) (vertical: bool) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable puzzle = puzzle
    let mutable word = word
    let mutable row = row
    let mutable col = col
    let mutable vertical = vertical
    try
        for i in 0 .. ((String.length(word)) - 1) do
            if vertical then
                if ((row + i) >= (Seq.length(puzzle))) || ((_idx (_idx puzzle (row + i)) (col)) <> "") then
                    __ret <- false
                    raise Return
            else
                if ((col + i) >= (Seq.length(_idx puzzle (0)))) || ((_idx (_idx puzzle (row)) (col + i)) <> "") then
                    __ret <- false
                    raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and place_word (puzzle: string array array) (word: string) (row: int) (col: int) (vertical: bool) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable puzzle = puzzle
    let mutable word = word
    let mutable row = row
    let mutable col = col
    let mutable vertical = vertical
    try
        for i in 0 .. ((String.length(word)) - 1) do
            let ch: string = string (word.[i])
            if vertical then
                puzzle.[row + i].[col] <- ch
            else
                puzzle.[row].[col + i] <- ch
        __ret
    with
        | Return -> __ret
and remove_word (puzzle: string array array) (word: string) (row: int) (col: int) (vertical: bool) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable puzzle = puzzle
    let mutable word = word
    let mutable row = row
    let mutable col = col
    let mutable vertical = vertical
    try
        for i in 0 .. ((String.length(word)) - 1) do
            if vertical then
                puzzle.[row + i].[col] <- ""
            else
                puzzle.[row].[col + i] <- ""
        __ret
    with
        | Return -> __ret
and solve_crossword (puzzle: string array array) (words: string array) (used: bool array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable puzzle = puzzle
    let mutable words = words
    let mutable used = used
    try
        for row in 0 .. ((Seq.length(puzzle)) - 1) do
            for col in 0 .. ((Seq.length(_idx puzzle (0))) - 1) do
                if (_idx (_idx puzzle (row)) (col)) = "" then
                    for i in 0 .. ((Seq.length(words)) - 1) do
                        if not (_idx used (i)) then
                            let word: string = _idx words (i)
                            for vertical in [|true; false|] do
                                if is_valid (puzzle) (word) (row) (col) (unbox<bool> vertical) then
                                    place_word (puzzle) (word) (row) (col) (unbox<bool> vertical)
                                    used.[i] <- true
                                    if solve_crossword (puzzle) (words) (used) then
                                        __ret <- true
                                        raise Return
                                    used.[i] <- false
                                    remove_word (puzzle) (word) (row) (col) (unbox<bool> vertical)
                    __ret <- false
                    raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let mutable puzzle: string array array = [|[|""; ""; ""|]; [|""; ""; ""|]; [|""; ""; ""|]|]
let mutable words: string array = [|"cat"; "dog"; "car"|]
let mutable used: bool array = [|false; false; false|]
if solve_crossword (puzzle) (words) (used) then
    printfn "%s" ("Solution found:")
    for row in puzzle do
        printfn "%s" (_repr (row))
else
    printfn "%s" ("No solution found:")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

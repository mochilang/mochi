// Generated 2025-08-06 20:48 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec contains (xs: int array) (x: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length(xs)) do
            if (_idx xs (i)) = x then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and get_point_key (len_board: int) (len_board_column: int) (row: int) (column: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable len_board = len_board
    let mutable len_board_column = len_board_column
    let mutable row = row
    let mutable column = column
    try
        __ret <- ((len_board * len_board_column) * row) + column
        raise Return
        __ret
    with
        | Return -> __ret
and search_from (board: string array array) (word: string) (row: int) (column: int) (word_index: int) (visited: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable board = board
    let mutable word = word
    let mutable row = row
    let mutable column = column
    let mutable word_index = word_index
    let mutable visited = visited
    try
        if (_idx (_idx board (row)) (column)) <> (_substring word word_index (word_index + 1)) then
            __ret <- false
            raise Return
        if word_index = ((String.length(word)) - 1) then
            __ret <- true
            raise Return
        let len_board: int = Seq.length(board)
        let len_board_column: int = Seq.length(_idx board (0))
        let dir_i: int array = [|0; 0; -1; 1|]
        let dir_j: int array = [|1; -1; 0; 0|]
        let mutable k: int = 0
        try
            while k < 4 do
                try
                    let next_i: int = row + (_idx dir_i (k))
                    let next_j: int = column + (_idx dir_j (k))
                    if not ((((0 <= next_i) && (next_i < len_board)) && (0 <= next_j)) && (next_j < len_board_column)) then
                        k <- k + 1
                        raise Continue
                    let key: int = get_point_key (len_board) (len_board_column) (next_i) (next_j)
                    if contains (visited) (key) then
                        k <- k + 1
                        raise Continue
                    let new_visited: int array = Array.append visited [|key|]
                    if search_from (board) (word) (next_i) (next_j) (word_index + 1) (new_visited) then
                        __ret <- true
                        raise Return
                    k <- k + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and word_exists (board: string array array) (word: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable board = board
    let mutable word = word
    try
        let len_board: int = Seq.length(board)
        let len_board_column: int = Seq.length(_idx board (0))
        let mutable i: int = 0
        while i < len_board do
            let mutable j: int = 0
            while j < len_board_column do
                let key: int = get_point_key (len_board) (len_board_column) (i) (j)
                let visited: int array = Array.append (Array.empty<int>) [|key|]
                if search_from (board) (word) (i) (j) (0) (visited) then
                    __ret <- true
                    raise Return
                j <- j + 1
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let board: string array array = [|[|"A"; "B"; "C"; "E"|]; [|"S"; "F"; "C"; "S"|]; [|"A"; "D"; "E"; "E"|]|]
        printfn "%b" (word_exists (board) ("ABCCED"))
        printfn "%b" (word_exists (board) ("SEE"))
        printfn "%b" (word_exists (board) ("ABCB"))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

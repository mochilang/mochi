// Generated 2025-08-09 10:14 +0700

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

let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Coord = {
    mutable _x: int
    mutable _y: int
}
type PlayResult = {
    mutable _matrix: string array array
    mutable _score: int
}
let rec is_alnum (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        __ret <- (((ch >= "0") && (ch <= "9")) || ((ch >= "A") && (ch <= "Z"))) || ((ch >= "a") && (ch <= "z"))
        raise Return
        __ret
    with
        | Return -> __ret
and to_int (token: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable token = token
    try
        let mutable res: int = 0
        let mutable i: int = 0
        while i < (String.length (token)) do
            res <- int (((int64 res) * (int64 10)) + (int64 (int (_substring token i (i + 1)))))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable res: string array = Array.empty<string>
        let mutable current: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s i (i + 1)
            if ch = sep then
                res <- Array.append res [|current|]
                current <- ""
            else
                current <- current + ch
            i <- i + 1
        res <- Array.append res [|current|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and parse_moves (input_str: string) =
    let mutable __ret : Coord array = Unchecked.defaultof<Coord array>
    let mutable input_str = input_str
    try
        let pairs: string array = split (input_str) (",")
        let mutable moves: Coord array = Array.empty<Coord>
        let mutable i: int = 0
        while i < (Seq.length (pairs)) do
            let pair: string = _idx pairs (int i)
            let mutable numbers: string array = Array.empty<string>
            let mutable num: string = ""
            let mutable j: int = 0
            while j < (String.length (pair)) do
                let ch: string = _substring pair j (j + 1)
                if ch = " " then
                    if num <> "" then
                        numbers <- Array.append numbers [|num|]
                        num <- ""
                else
                    num <- num + ch
                j <- j + 1
            if num <> "" then
                numbers <- Array.append numbers [|num|]
            if (Seq.length (numbers)) <> 2 then
                failwith ("Each move must have exactly two numbers.")
            let _x: int = to_int (_idx numbers (int 0))
            let _y: int = to_int (_idx numbers (int 1))
            moves <- Array.append moves [|{ _x = _x; _y = _y }|]
            i <- i + 1
        __ret <- moves
        raise Return
        __ret
    with
        | Return -> __ret
and validate_matrix_size (size: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable size = size
    try
        if size <= 0 then
            failwith ("Matrix size must be a positive integer.")
        __ret
    with
        | Return -> __ret
and validate_matrix_content (_matrix: string array) (size: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable _matrix = _matrix
    let mutable size = size
    try
        if (Seq.length (_matrix)) <> size then
            failwith ("The matrix dont match with size.")
        let mutable i: int = 0
        while i < size do
            let mutable row: string = _idx _matrix (int i)
            if (String.length (row)) <> size then
                failwith (("Each row in the matrix must have exactly " + (_str (size))) + " characters.")
            let mutable j: int = 0
            while j < size do
                let ch: string = _substring row j (j + 1)
                if not (is_alnum (ch)) then
                    failwith ("Matrix rows can only contain letters and numbers.")
                j <- j + 1
            i <- i + 1
        __ret
    with
        | Return -> __ret
and validate_moves (moves: Coord array) (size: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable moves = moves
    let mutable size = size
    try
        let mutable i: int = 0
        while i < (Seq.length (moves)) do
            let mv: Coord = _idx moves (int i)
            if ((((mv._x) < 0) || ((mv._x) >= size)) || ((mv._y) < 0)) || ((mv._y) >= size) then
                failwith ("Move is out of bounds for a matrix.")
            i <- i + 1
        __ret
    with
        | Return -> __ret
and contains (pos: Coord array) (r: int) (c: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable pos = pos
    let mutable r = r
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (Seq.length (pos)) do
            let p: Coord = _idx pos (int i)
            if ((p._x) = r) && ((p._y) = c) then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and find_repeat (matrix_g: string array array) (row: int) (column: int) (size: int) =
    let mutable __ret : Coord array = Unchecked.defaultof<Coord array>
    let mutable matrix_g = matrix_g
    let mutable row = row
    let mutable column = column
    let mutable size = size
    try
        column <- (size - 1) - column
        let mutable visited: Coord array = Array.empty<Coord>
        let mutable repeated: Coord array = Array.empty<Coord>
        let color: string = _idx (_idx matrix_g (int column)) (int row)
        if color = "-" then
            __ret <- repeated
            raise Return
        let mutable stack: Coord array = unbox<Coord array> [|{ _x = column; _y = row }|]
        try
            while (Seq.length (stack)) > 0 do
                try
                    let idx: int = (Seq.length (stack)) - 1
                    let pos: Coord = _idx stack (int idx)
                    stack <- Array.sub stack 0 (idx - 0)
                    if ((((pos._x) < 0) || ((pos._x) >= size)) || ((pos._y) < 0)) || ((pos._y) >= size) then
                        raise Continue
                    if contains (visited) (pos._x) (pos._y) then
                        raise Continue
                    visited <- Array.append visited [|pos|]
                    if (_idx (_idx matrix_g (int (pos._x))) (int (pos._y))) = color then
                        repeated <- Array.append repeated [|pos|]
                        stack <- Array.append stack [|{ _x = (pos._x) - 1; _y = pos._y }|]
                        stack <- Array.append stack [|{ _x = (pos._x) + 1; _y = pos._y }|]
                        stack <- Array.append stack [|{ _x = pos._x; _y = (pos._y) - 1 }|]
                        stack <- Array.append stack [|{ _x = pos._x; _y = (pos._y) + 1 }|]
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- repeated
        raise Return
        __ret
    with
        | Return -> __ret
and increment_score (count: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable count = count
    try
        __ret <- int (((int64 count) * (int64 (count + 1))) / (int64 2))
        raise Return
        __ret
    with
        | Return -> __ret
and move_x (matrix_g: string array array) (column: int) (size: int) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable matrix_g = matrix_g
    let mutable column = column
    let mutable size = size
    try
        let mutable new_list: string array = Array.empty<string>
        let mutable row: int = 0
        while row < size do
            let ``val``: string = _idx (_idx matrix_g (int row)) (int column)
            if ``val`` <> "-" then
                new_list <- Array.append new_list [|``val``|]
            else
                new_list <- unbox<string array> (Array.append ([|``val``|]) (new_list))
            row <- row + 1
        row <- 0
        while row < size do
            matrix_g.[int row].[int column] <- _idx new_list (int row)
            row <- row + 1
        __ret <- matrix_g
        raise Return
        __ret
    with
        | Return -> __ret
and move_y (matrix_g: string array array) (size: int) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable matrix_g = matrix_g
    let mutable size = size
    try
        let mutable empty_cols: int array = Array.empty<int>
        let mutable column: int = size - 1
        try
            while column >= 0 do
                try
                    let mutable row: int = 0
                    let mutable all_empty: bool = true
                    try
                        while row < size do
                            try
                                if (_idx (_idx matrix_g (int row)) (int column)) <> "-" then
                                    all_empty <- false
                                    raise Break
                                row <- row + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if all_empty then
                        empty_cols <- Array.append empty_cols [|column|]
                    column <- column - 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable i: int = 0
        while i < (Seq.length (empty_cols)) do
            let col: int = _idx empty_cols (int i)
            let mutable c: int = col + 1
            while c < size do
                let mutable r: int = 0
                while r < size do
                    matrix_g.[int r].[int (c - 1)] <- _idx (_idx matrix_g (int r)) (int c)
                    r <- r + 1
                c <- c + 1
            let mutable r: int = 0
            while r < size do
                matrix_g.[int r].[int (size - 1)] <- "-"
                r <- r + 1
            i <- i + 1
        __ret <- matrix_g
        raise Return
        __ret
    with
        | Return -> __ret
and play (matrix_g: string array array) (pos_x: int) (pos_y: int) (size: int) =
    let mutable __ret : PlayResult = Unchecked.defaultof<PlayResult>
    let mutable matrix_g = matrix_g
    let mutable pos_x = pos_x
    let mutable pos_y = pos_y
    let mutable size = size
    try
        let same_colors: Coord array = find_repeat (matrix_g) (pos_x) (pos_y) (size)
        if (Seq.length (same_colors)) <> 0 then
            let mutable i: int = 0
            while i < (Seq.length (same_colors)) do
                let p: Coord = _idx same_colors (int i)
                matrix_g.[int (p._x)].[int (p._y)] <- "-"
                i <- i + 1
            let mutable column: int = 0
            while column < size do
                matrix_g <- move_x (matrix_g) (column) (size)
                column <- column + 1
            matrix_g <- move_y (matrix_g) (size)
        let sc: int = increment_score (Seq.length (same_colors))
        __ret <- { _matrix = matrix_g; _score = sc }
        raise Return
        __ret
    with
        | Return -> __ret
and build_matrix (_matrix: string array) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable _matrix = _matrix
    try
        let mutable res: string array array = Array.empty<string array>
        let mutable i: int = 0
        while i < (Seq.length (_matrix)) do
            let mutable row: string = _idx _matrix (int i)
            let mutable row_list: string array = Array.empty<string>
            let mutable j: int = 0
            while j < (String.length (row)) do
                row_list <- Array.append row_list [|(_substring row j (j + 1))|]
                j <- j + 1
            res <- Array.append res [|row_list|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and process_game (size: int) (_matrix: string array) (moves: Coord array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable size = size
    let mutable _matrix = _matrix
    let mutable moves = moves
    try
        let mutable game_matrix: string array array = build_matrix (_matrix)
        let mutable total: int = 0
        let mutable i: int = 0
        while i < (Seq.length (moves)) do
            let mv: Coord = _idx moves (int i)
            let mutable res: PlayResult = play (game_matrix) (mv._x) (mv._y) (size)
            game_matrix <- res._matrix
            total <- total + (res._score)
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let size: int = 4
        let _matrix: string array = unbox<string array> [|"RRBG"; "RBBG"; "YYGG"; "XYGG"|]
        let mutable moves: Coord array = parse_moves ("0 1,1 1")
        validate_matrix_size (size)
        validate_matrix_content (_matrix) (size)
        validate_moves (moves) (size)
        let _score: int = process_game (size) (_matrix) (moves)
        printfn "%s" (_str (_score))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

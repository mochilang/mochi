// Generated 2025-08-09 16:21 +0700

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
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
type WordSearch = {
    mutable _words: string array
    mutable _width: int
    mutable _height: int
    mutable _board: string array array
}
let mutable _seed: int = 123456789
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        _seed <- int ((((((int64 _seed) * (int64 1103515245)) + (int64 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- _seed
        raise Return
        __ret
    with
        | Return -> __ret
and rand_range (max: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable max = max
    try
        __ret <- (((rand()) % max + max) % max)
        raise Return
        __ret
    with
        | Return -> __ret
and shuffle (list_int: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable list_int = list_int
    try
        let mutable i: int = (Seq.length (list_int)) - 1
        while i > 0 do
            let j: int = rand_range (i + 1)
            let tmp: int = _idx list_int (int i)
            list_int.[int i] <- _idx list_int (int j)
            list_int.[int j] <- tmp
            i <- i - 1
        __ret <- list_int
        raise Return
        __ret
    with
        | Return -> __ret
and rand_letter () =
    let mutable __ret : string = Unchecked.defaultof<string>
    try
        let letters: string = "abcdefghijklmnopqrstuvwxyz"
        let mutable i: int = rand_range (26)
        __ret <- _substring letters (i) (i + 1)
        raise Return
        __ret
    with
        | Return -> __ret
and make_word_search (_words: string array) (_width: int) (_height: int) =
    let mutable __ret : WordSearch = Unchecked.defaultof<WordSearch>
    let mutable _words = _words
    let mutable _width = _width
    let mutable _height = _height
    try
        let mutable _board: string array array = Array.empty<string array>
        let mutable r: int = 0
        while r < _height do
            let mutable row: string array = Array.empty<string>
            let mutable c: int = 0
            while c < _width do
                row <- Array.append row [|""|]
                c <- c + 1
            _board <- Array.append _board [|row|]
            r <- r + 1
        __ret <- { _words = _words; _width = _width; _height = _height; _board = _board }
        raise Return
        __ret
    with
        | Return -> __ret
and insert_dir (ws: WordSearch) (word: string) (dr: int) (dc: int) (rows: int array) (cols: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ws = ws
    let mutable word = word
    let mutable dr = dr
    let mutable dc = dc
    let mutable rows = rows
    let mutable cols = cols
    try
        let word_len: int = String.length (word)
        let mutable ri: int = 0
        try
            while ri < (Seq.length (rows)) do
                try
                    let mutable row: int = _idx rows (int ri)
                    let mutable ci: int = 0
                    try
                        while ci < (Seq.length (cols)) do
                            try
                                let col: int = _idx cols (int ci)
                                let end_r: int64 = (int64 row) + ((int64 dr) * (int64 (word_len - 1)))
                                let end_c: int64 = (int64 col) + ((int64 dc) * (int64 (word_len - 1)))
                                if (((end_r < (int64 0)) || (end_r >= (int64 (ws._height)))) || (end_c < (int64 0))) || (end_c >= (int64 (ws._width))) then
                                    ci <- ci + 1
                                    raise Continue
                                let mutable k: int = 0
                                let mutable ok: bool = true
                                try
                                    while k < word_len do
                                        try
                                            let rr: int64 = (int64 row) + ((int64 dr) * (int64 k))
                                            let cc: int64 = (int64 col) + ((int64 dc) * (int64 k))
                                            if (_idx (_idx (ws._board) (int rr)) (int cc)) <> "" then
                                                ok <- false
                                                raise Break
                                            k <- k + 1
                                        with
                                        | Continue -> ()
                                        | Break -> raise Break
                                with
                                | Break -> ()
                                | Continue -> ()
                                if ok then
                                    k <- 0
                                    while k < word_len do
                                        let rr2: int64 = (int64 row) + ((int64 dr) * (int64 k))
                                        let cc2: int64 = (int64 col) + ((int64 dc) * (int64 k))
                                        let mutable row_list: string array = _idx (ws._board) (int rr2)
                                        row_list.[int cc2] <- _substring word (k) (k + 1)
                                        k <- k + 1
                                    __ret <- true
                                    raise Return
                                ci <- ci + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    ri <- ri + 1
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
and generate_board (ws: WordSearch) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable ws = ws
    try
        let dirs_r: int array = unbox<int array> [|-1; -1; 0; 1; 1; 1; 0; -1|]
        let dirs_c: int array = unbox<int array> [|0; 1; 1; 1; 0; -1; -1; -1|]
        let mutable i: int = 0
        while i < (Seq.length (ws._words)) do
            let word: string = _idx (ws._words) (int i)
            let mutable rows: int array = Array.empty<int>
            let mutable r: int = 0
            while r < (ws._height) do
                rows <- Array.append rows [|r|]
                r <- r + 1
            let mutable cols: int array = Array.empty<int>
            let mutable c: int = 0
            while c < (ws._width) do
                cols <- Array.append cols [|c|]
                c <- c + 1
            rows <- shuffle (rows)
            cols <- shuffle (cols)
            let d: int = rand_range (8)
            insert_dir (ws) (word) (_idx dirs_r (int d)) (_idx dirs_c (int d)) (rows) (cols)
            i <- i + 1
        __ret
    with
        | Return -> __ret
and visualise (ws: WordSearch) (add_fake_chars: bool) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ws = ws
    let mutable add_fake_chars = add_fake_chars
    try
        let mutable result: string = ""
        let mutable r: int = 0
        while r < (ws._height) do
            let mutable c: int = 0
            while c < (ws._width) do
                let mutable ch: string = _idx (_idx (ws._board) (int r)) (int c)
                if ch = "" then
                    if add_fake_chars then
                        ch <- rand_letter()
                    else
                        ch <- "#"
                result <- (result + ch) + " "
                c <- c + 1
            result <- result + "\n"
            r <- r + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let _words: string array = unbox<string array> [|"cat"; "dog"; "snake"; "fish"|]
        let mutable ws: WordSearch = make_word_search (_words) (10) (10)
        generate_board (ws)
        printfn "%s" (visualise (ws) (true))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

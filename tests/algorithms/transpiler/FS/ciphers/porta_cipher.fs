// Generated 2025-08-07 10:31 +0700

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
let UPPER: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let LOWER: string = "abcdefghijklmnopqrstuvwxyz"
let BASE_TOP: string = "ABCDEFGHIJKLM"
let BASE_BOTTOM: string = "NOPQRSTUVWXYZ"
let rec to_upper (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable res: string = ""
        let mutable i: int = 0
        try
            while i < (String.length (s)) do
                try
                    let ch: string = _substring s i (i + 1)
                    let mutable j: int = 0
                    let mutable replaced: bool = false
                    try
                        while j < (String.length (LOWER)) do
                            try
                                if (_substring LOWER j (j + 1)) = ch then
                                    res <- res + (_substring UPPER j (j + 1))
                                    replaced <- true
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if not replaced then
                        res <- res + ch
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and char_index (c: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (String.length (UPPER)) do
            if (_substring UPPER i (i + 1)) = c then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and rotate_right (s: string) (k: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable k = k
    try
        let n: int = String.length (s)
        let shift: int = ((k % n + n) % n)
        __ret <- (_substring s (n - shift) n) + (_substring s 0 (n - shift))
        raise Return
        __ret
    with
        | Return -> __ret
and table_for (c: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable c = c
    try
        let idx: int = char_index (c)
        let shift: int = idx / 2
        let row1: string = rotate_right (BASE_BOTTOM) (shift)
        let pair: string array = [|BASE_TOP; row1|]
        __ret <- pair
        raise Return
        __ret
    with
        | Return -> __ret
and generate_table (key: string) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable key = key
    try
        let up: string = to_upper (key)
        let mutable i: int = 0
        let mutable result: string array array = [||]
        while i < (String.length (up)) do
            let ch: string = _substring up i (i + 1)
            let pair: string array = table_for (ch)
            result <- Array.append result [|pair|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and str_index (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- 0 - 1
        raise Return
        __ret
    with
        | Return -> __ret
and get_position (table: string array) (ch: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable table = table
    let mutable ch = ch
    try
        let mutable row: int = 0
        if (str_index (_idx table (0)) (ch)) = (0 - 1) then
            row <- 1
        let col: int = str_index (_idx table (row)) (ch)
        __ret <- unbox<int array> [|row; col|]
        raise Return
        __ret
    with
        | Return -> __ret
and get_opponent (table: string array) (ch: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable table = table
    let mutable ch = ch
    try
        let pos: int array = get_position (table) (ch)
        let mutable row: int = _idx pos (0)
        let col: int = _idx pos (1)
        if col = (0 - 1) then
            __ret <- ch
            raise Return
        if row = 1 then
            __ret <- _substring (_idx table (0)) col (col + 1)
            raise Return
        __ret <- _substring (_idx table (1)) col (col + 1)
        raise Return
        __ret
    with
        | Return -> __ret
and encrypt (key: string) (words: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable words = words
    try
        let table: string array array = generate_table (key)
        let up_words: string = to_upper (words)
        let mutable cipher: string = ""
        let mutable count: int = 0
        let mutable i: int = 0
        while i < (String.length (up_words)) do
            let ch: string = _substring up_words i (i + 1)
            cipher <- cipher + (get_opponent (_idx table (count)) (ch))
            count <- (((count + 1) % (Seq.length (table)) + (Seq.length (table))) % (Seq.length (table)))
            i <- i + 1
        __ret <- cipher
        raise Return
        __ret
    with
        | Return -> __ret
and decrypt (key: string) (words: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable words = words
    try
        let mutable res: string = encrypt (key) (words)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (encrypt ("marvin") ("jessica"))
        printfn "%s" (decrypt ("marvin") ("QRACRWU"))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

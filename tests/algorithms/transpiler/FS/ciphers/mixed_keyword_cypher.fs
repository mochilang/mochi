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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let UPPER: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let LOWER: string = "abcdefghijklmnopqrstuvwxyz"
let rec to_upper (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable res: string = ""
        let mutable i: int = 0
        try
            while i < (String.length (s)) do
                try
                    let ch: string = string (s.[i])
                    let mutable j: int = 0
                    let mutable found: bool = false
                    try
                        while j < 26 do
                            try
                                if ch = (string (LOWER.[j])) then
                                    res <- res + (string (UPPER.[j]))
                                    found <- true
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if found = false then
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
let rec contains (xs: string array) (x: string) =
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
let rec contains_char (s: string) (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (string (s.[i])) = ch then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_value (keys: string array) (values: string array) (key: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable keys = keys
    let mutable values = values
    let mutable key = key
    try
        let mutable i: int = 0
        while i < (Seq.length (keys)) do
            if (_idx keys (i)) = key then
                __ret <- _idx values (i)
                raise Return
            i <- i + 1
        __ret <- unbox<string> null
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_mapping (keys: string array) (values: string array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable keys = keys
    let mutable values = values
    try
        let mutable s: string = "{"
        let mutable i: int = 0
        while i < (Seq.length (keys)) do
            s <- ((((s + "'") + (_idx keys (i))) + "': '") + (_idx values (i))) + "'"
            if (i + 1) < (Seq.length (keys)) then
                s <- s + ", "
            i <- i + 1
        s <- s + "}"
        printfn "%s" (s)
        __ret
    with
        | Return -> __ret
let rec mixed_keyword (keyword: string) (plaintext: string) (verbose: bool) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable keyword = keyword
    let mutable plaintext = plaintext
    let mutable verbose = verbose
    try
        let alphabet: string = UPPER
        let keyword_u: string = to_upper (keyword)
        let plaintext_u: string = to_upper (plaintext)
        let mutable unique: string array = [||]
        let mutable i: int = 0
        while i < (String.length (keyword_u)) do
            let ch: string = string (keyword_u.[i])
            if (contains_char (alphabet) (ch)) && ((contains (unique) (ch)) = false) then
                unique <- Array.append unique [|ch|]
            i <- i + 1
        let num_unique: int = Seq.length (unique)
        let mutable shifted: string array = [||]
        i <- 0
        while i < (Seq.length (unique)) do
            shifted <- Array.append shifted [|_idx unique (i)|]
            i <- i + 1
        i <- 0
        while i < (String.length (alphabet)) do
            let ch: string = string (alphabet.[i])
            if (contains (unique) (ch)) = false then
                shifted <- Array.append shifted [|ch|]
            i <- i + 1
        let mutable modified: string array array = [||]
        let mutable k: int = 0
        while k < (Seq.length (shifted)) do
            let mutable row: string array = [||]
            let mutable r: int = 0
            while (r < num_unique) && ((k + r) < (Seq.length (shifted))) do
                row <- Array.append row [|_idx shifted (k + r)|]
                r <- r + 1
            modified <- Array.append modified [|row|]
            k <- k + num_unique
        let mutable keys: string array = [||]
        let mutable values: string array = [||]
        let mutable column: int = 0
        let mutable letter_index: int = 0
        try
            while column < num_unique do
                try
                    let mutable row_idx: int = 0
                    try
                        while row_idx < (Seq.length (modified)) do
                            try
                                let mutable row: string array = _idx modified (row_idx)
                                if (Seq.length (row)) <= column then
                                    raise Break
                                keys <- Array.append keys [|string (alphabet.[letter_index])|]
                                values <- Array.append values [|_idx row (column)|]
                                letter_index <- letter_index + 1
                                row_idx <- row_idx + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    column <- column + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        if verbose then
            print_mapping (keys) (values)
        let mutable result: string = ""
        i <- 0
        while i < (String.length (plaintext_u)) do
            let ch: string = string (plaintext_u.[i])
            let mapped: string = get_value (keys) (values) (ch)
            if mapped = (unbox<string> null) then
                result <- result + ch
            else
                result <- result + mapped
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (mixed_keyword ("college") ("UNIVERSITY") (true))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

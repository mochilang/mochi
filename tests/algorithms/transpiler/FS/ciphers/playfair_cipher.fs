// Generated 2025-08-07 10:31 +0700

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
and index_of (xs: string array) (x: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) = x then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and prepare_input (dirty: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable dirty = dirty
    try
        let letters: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let upper_dirty: string = dirty.ToUpper()
        let mutable filtered: string = ""
        let mutable i: int = 0
        while i < (String.length (upper_dirty)) do
            let c: string = _substring upper_dirty i (i + 1)
            if letters.Contains(c) then
                filtered <- filtered + c
            i <- i + 1
        if (String.length (filtered)) < 2 then
            __ret <- filtered
            raise Return
        let mutable clean: string = ""
        i <- 0
        while i < ((String.length (filtered)) - 1) do
            let c1: string = _substring filtered i (i + 1)
            let c2: string = _substring filtered (i + 1) (i + 2)
            clean <- clean + c1
            if c1 = c2 then
                clean <- clean + "X"
            i <- i + 1
        clean <- clean + (_substring filtered ((String.length (filtered)) - 1) (String.length (filtered)))
        if ((((String.length (clean)) % 2 + 2) % 2)) = 1 then
            clean <- clean + "X"
        __ret <- clean
        raise Return
        __ret
    with
        | Return -> __ret
and generate_table (key: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable key = key
    try
        let alphabet: string = "ABCDEFGHIKLMNOPQRSTUVWXYZ"
        let mutable table: string array = [||]
        let upper_key: string = key.ToUpper()
        let mutable i: int = 0
        while i < (String.length (upper_key)) do
            let c: string = _substring upper_key i (i + 1)
            if alphabet.Contains(c) then
                if not (contains (table) (c)) then
                    table <- Array.append table [|c|]
            i <- i + 1
        i <- 0
        while i < (String.length (alphabet)) do
            let c: string = _substring alphabet i (i + 1)
            if not (contains (table) (c)) then
                table <- Array.append table [|c|]
            i <- i + 1
        __ret <- table
        raise Return
        __ret
    with
        | Return -> __ret
and encode (plaintext: string) (key: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable plaintext = plaintext
    let mutable key = key
    try
        let mutable table: string array = generate_table (key)
        let text: string = prepare_input (plaintext)
        let mutable cipher: string = ""
        let mutable i: int = 0
        while i < (String.length (text)) do
            let c1: string = _substring text i (i + 1)
            let c2: string = _substring text (i + 1) (i + 2)
            let idx1: int = index_of (table) (c1)
            let idx2: int = index_of (table) (c2)
            let row1: int = idx1 / 5
            let col1: int = ((idx1 % 5 + 5) % 5)
            let row2: int = idx2 / 5
            let col2: int = ((idx2 % 5 + 5) % 5)
            if row1 = row2 then
                cipher <- cipher + (_idx table ((row1 * 5) + ((((col1 + 1) % 5 + 5) % 5))))
                cipher <- cipher + (_idx table ((row2 * 5) + ((((col2 + 1) % 5 + 5) % 5))))
            else
                if col1 = col2 then
                    cipher <- cipher + (_idx table ((((((row1 + 1) % 5 + 5) % 5)) * 5) + col1))
                    cipher <- cipher + (_idx table ((((((row2 + 1) % 5 + 5) % 5)) * 5) + col2))
                else
                    cipher <- cipher + (_idx table ((row1 * 5) + col2))
                    cipher <- cipher + (_idx table ((row2 * 5) + col1))
            i <- i + 2
        __ret <- cipher
        raise Return
        __ret
    with
        | Return -> __ret
and decode (cipher: string) (key: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable cipher = cipher
    let mutable key = key
    try
        let mutable table: string array = generate_table (key)
        let mutable plain: string = ""
        let mutable i: int = 0
        while i < (String.length (cipher)) do
            let c1: string = _substring cipher i (i + 1)
            let c2: string = _substring cipher (i + 1) (i + 2)
            let idx1: int = index_of (table) (c1)
            let idx2: int = index_of (table) (c2)
            let row1: int = idx1 / 5
            let col1: int = ((idx1 % 5 + 5) % 5)
            let row2: int = idx2 / 5
            let col2: int = ((idx2 % 5 + 5) % 5)
            if row1 = row2 then
                plain <- plain + (_idx table ((row1 * 5) + ((((col1 + 4) % 5 + 5) % 5))))
                plain <- plain + (_idx table ((row2 * 5) + ((((col2 + 4) % 5 + 5) % 5))))
            else
                if col1 = col2 then
                    plain <- plain + (_idx table ((((((row1 + 4) % 5 + 5) % 5)) * 5) + col1))
                    plain <- plain + (_idx table ((((((row2 + 4) % 5 + 5) % 5)) * 5) + col2))
                else
                    plain <- plain + (_idx table ((row1 * 5) + col2))
                    plain <- plain + (_idx table ((row2 * 5) + col1))
            i <- i + 2
        __ret <- plain
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (String.concat " " ([|sprintf "%s" ("Encoded:"); sprintf "%s" (encode ("BYE AND THANKS") ("GREETING"))|]))
        printfn "%s" (String.concat " " ([|sprintf "%s" ("Decoded:"); sprintf "%s" (decode ("CXRBANRLBALQ") ("GREETING"))|]))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

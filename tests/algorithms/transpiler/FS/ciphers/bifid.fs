// Generated 2025-08-06 23:33 +0700

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
let SQUARE: string array array = [|[|"a"; "b"; "c"; "d"; "e"|]; [|"f"; "g"; "h"; "i"; "k"|]; [|"l"; "m"; "n"; "o"; "p"|]; [|"q"; "r"; "s"; "t"; "u"|]; [|"v"; "w"; "x"; "y"; "z"|]|]
let rec index_of (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (string (s.[i])) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and to_lower_without_spaces (message: string) (replace_j: bool) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    let mutable replace_j = replace_j
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length (message)) do
            let mutable ch: string = string (message.[i])
            let pos: int = index_of (upper) (ch)
            if pos >= 0 then
                ch <- string (lower.[pos])
            if ch <> " " then
                if replace_j && (ch = "j") then
                    ch <- "i"
                res <- res + ch
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and letter_to_numbers (letter: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable letter = letter
    try
        let mutable r: int = 0
        while r < (Seq.length (SQUARE)) do
            let mutable c: int = 0
            while c < (Seq.length (_idx SQUARE (r))) do
                if (_idx (_idx SQUARE (r)) (c)) = letter then
                    __ret <- unbox<int array> [|r + 1; c + 1|]
                    raise Return
                c <- c + 1
            r <- r + 1
        __ret <- unbox<int array> [|0; 0|]
        raise Return
        __ret
    with
        | Return -> __ret
and numbers_to_letter (row: int) (col: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable row = row
    let mutable col = col
    try
        __ret <- _idx (_idx SQUARE (row - 1)) (col - 1)
        raise Return
        __ret
    with
        | Return -> __ret
and encode (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    try
        let clean: string = to_lower_without_spaces (message) (true)
        let l: int = String.length (clean)
        let mutable rows: int array = Array.empty<int>
        let mutable cols: int array = Array.empty<int>
        let mutable i: int = 0
        while i < l do
            let nums: int array = letter_to_numbers (string (clean.[i]))
            rows <- Array.append rows [|_idx nums (0)|]
            cols <- Array.append cols [|_idx nums (1)|]
            i <- i + 1
        let mutable seq: int array = Array.empty<int>
        i <- 0
        while i < l do
            seq <- Array.append seq [|_idx rows (i)|]
            i <- i + 1
        i <- 0
        while i < l do
            seq <- Array.append seq [|_idx cols (i)|]
            i <- i + 1
        let mutable encoded: string = ""
        i <- 0
        while i < l do
            let mutable r: int = _idx seq (2 * i)
            let mutable c: int = _idx seq ((2 * i) + 1)
            encoded <- encoded + (numbers_to_letter (r) (c))
            i <- i + 1
        __ret <- encoded
        raise Return
        __ret
    with
        | Return -> __ret
and decode (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    try
        let clean: string = to_lower_without_spaces (message) (false)
        let l: int = String.length (clean)
        let mutable first: int array = Array.empty<int>
        let mutable i: int = 0
        while i < l do
            let nums: int array = letter_to_numbers (string (clean.[i]))
            first <- Array.append first [|_idx nums (0)|]
            first <- Array.append first [|_idx nums (1)|]
            i <- i + 1
        let mutable top: int array = Array.empty<int>
        let mutable bottom: int array = Array.empty<int>
        i <- 0
        while i < l do
            top <- Array.append top [|_idx first (i)|]
            bottom <- Array.append bottom [|_idx first (i + l)|]
            i <- i + 1
        let mutable decoded: string = ""
        i <- 0
        while i < l do
            let mutable r: int = _idx top (i)
            let mutable c: int = _idx bottom (i)
            decoded <- decoded + (numbers_to_letter (r) (c))
            i <- i + 1
        __ret <- decoded
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (encode ("testmessage"))
printfn "%s" (encode ("Test Message"))
printfn "%s" (encode ("test j"))
printfn "%s" (encode ("test i"))
printfn "%s" (decode ("qtltbdxrxlk"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

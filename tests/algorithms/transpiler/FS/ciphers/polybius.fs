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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let square: string array array = [|[|"a"; "b"; "c"; "d"; "e"|]; [|"f"; "g"; "h"; "i"; "k"|]; [|"l"; "m"; "n"; "o"; "p"|]; [|"q"; "r"; "s"; "t"; "u"|]; [|"v"; "w"; "x"; "y"; "z"|]|]
let rec letter_to_numbers (letter: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable letter = letter
    try
        let mutable i: int = 0
        while i < (Seq.length (square)) do
            let mutable j: int = 0
            while j < (Seq.length (_idx square (i))) do
                if (_idx (_idx square (i)) (j)) = letter then
                    __ret <- unbox<int array> [|i + 1; j + 1|]
                    raise Return
                j <- j + 1
            i <- i + 1
        __ret <- unbox<int array> [|0; 0|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec numbers_to_letter (index1: int) (index2: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable index1 = index1
    let mutable index2 = index2
    try
        __ret <- _idx (_idx square (index1 - 1)) (index2 - 1)
        raise Return
        __ret
    with
        | Return -> __ret
let rec char_to_int (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        if ch = "1" then
            __ret <- 1
            raise Return
        if ch = "2" then
            __ret <- 2
            raise Return
        if ch = "3" then
            __ret <- 3
            raise Return
        if ch = "4" then
            __ret <- 4
            raise Return
        if ch = "5" then
            __ret <- 5
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec encode (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    try
        message <- unbox<string> (message.ToLower())
        let mutable encoded: string = ""
        let mutable i: int = 0
        while i < (String.length (message)) do
            let mutable ch: string = string (message.[i])
            if ch = "j" then
                ch <- "i"
            if ch <> " " then
                let nums: int array = letter_to_numbers (ch)
                encoded <- (encoded + (_str (_idx nums (0)))) + (_str (_idx nums (1)))
            else
                encoded <- encoded + " "
            i <- i + 1
        __ret <- encoded
        raise Return
        __ret
    with
        | Return -> __ret
let rec decode (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    try
        let mutable decoded: string = ""
        let mutable i: int = 0
        while i < (String.length (message)) do
            if (string (message.[i])) = " " then
                decoded <- decoded + " "
                i <- i + 1
            else
                let index1: int = char_to_int (string (message.[i]))
                let index2: int = char_to_int (string (message.[i + 1]))
                let letter: string = numbers_to_letter (index1) (index2)
                decoded <- decoded + letter
                i <- i + 2
        __ret <- decoded
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (encode ("test message"))
printfn "%s" (encode ("Test Message"))
printfn "%s" (decode ("44154344 32154343112215"))
printfn "%s" (decode ("4415434432154343112215"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

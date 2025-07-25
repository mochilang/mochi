// Generated 2025-07-25 10:58 +0000

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable parts: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            if (((String.length sep) > 0) && ((i + (String.length sep)) <= (String.length s))) && ((s.Substring(i, (i + (String.length sep)) - i)) = sep) then
                parts <- Array.append parts [|cur|]
                cur <- ""
                i <- i + (String.length sep)
            else
                cur <- cur + (s.Substring(i, (i + 1) - i))
                i <- i + 1
        parts <- Array.append parts [|cur|]
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
let rec rstripEmpty (words: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable words = words
    try
        let mutable n: int = Array.length words
        while (n > 0) && ((unbox<string> (words.[n - 1])) = "") do
            n <- n - 1
        __ret <- Array.sub words 0 (n - 0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec spaces (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < n do
            out <- out + " "
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec pad (word: string) (width: int) (align: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable word = word
    let mutable width = width
    let mutable align = align
    try
        let diff: int = width - (String.length word)
        if align = 0 then
            __ret <- word + (unbox<string> (spaces diff))
            raise Return
        if align = 2 then
            __ret <- (unbox<string> (spaces diff)) + word
            raise Return
        let mutable left: int = unbox<int> (diff / 2)
        let mutable right: int = diff - left
        __ret <- ((unbox<string> (spaces left)) + word) + (unbox<string> (spaces right))
        raise Return
        __ret
    with
        | Return -> __ret
let rec newFormatter (text: string) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable text = text
    try
        let mutable lines: string array = split text "\n"
        let mutable fmtLines: string array array = [||]
        let mutable width: int array = [||]
        let mutable i: int = 0
        try
            while i < (unbox<int> (Array.length lines)) do
                if (Seq.length (lines.[i])) = 0 then
                    i <- i + 1
                    raise Continue
                let mutable words: string array = rstripEmpty (unbox<string array> (split (unbox<string> (lines.[i])) "$"))
                fmtLines <- Array.append fmtLines [|words|]
                let mutable j: int = 0
                while j < (unbox<int> (Array.length words)) do
                    let wlen: int = Seq.length (words.[j])
                    if j = (unbox<int> (Array.length width)) then
                        width <- Array.append width [|wlen|]
                    else
                        if wlen > (unbox<int> (width.[j])) then
                            width.[j] <- wlen
                    j <- j + 1
                i <- i + 1
        with
        | Break -> ()
        | Continue -> ()
        __ret <- Map.ofList [("text", box fmtLines); ("width", box width)]
        raise Return
        __ret
    with
        | Return -> __ret
let rec printFmt (f: Map<string, obj>) (align: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable f = f
    let mutable align = align
    try
        let lines: string array array = unbox<string array array> (f.["text"])
        let width: int array = unbox<int array> (f.["width"])
        let mutable i: int = 0
        while i < (unbox<int> (Array.length lines)) do
            let words: string array = lines.[i]
            let mutable line: string = ""
            let mutable j: int = 0
            while j < (unbox<int> (Array.length words)) do
                line <- (line + (unbox<string> (pad (unbox<string> (words.[j])) (unbox<int> (width.[j])) align))) + " "
                j <- j + 1
            printfn "%s" line
            i <- i + 1
        printfn "%s" ""
        __ret
    with
        | Return -> __ret
let text: string = (((("Given$a$text$file$of$many$lines,$where$fields$within$a$line\n" + "are$delineated$by$a$single$'dollar'$character,$write$a$program\n") + "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each\n") + "column$are$separated$by$at$least$one$space.\n") + "Further,$allow$for$each$word$in$a$column$to$be$either$left\n") + "justified,$right$justified,$or$center$justified$within$its$column."
let f: Map<string, obj> = newFormatter text
printFmt f 0
printFmt f 1
printFmt f 2
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

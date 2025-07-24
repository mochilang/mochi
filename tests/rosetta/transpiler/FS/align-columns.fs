// Generated 2025-07-24 20:52 +0700

exception Break
exception Continue

exception Return

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
and rstripEmpty (words: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable words = words
    try
        let mutable n: int = Seq.length words
        while (n > 0) && ((words.[n - 1]) = "") do
            n <- n - 1
        __ret <- Array.sub words 0 (n - 0)
        raise Return
        __ret
    with
        | Return -> __ret
and spaces (n: int) =
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
and pad (word: string) (width: int) (align: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable word = word
    let mutable width = width
    let mutable align = align
    try
        let diff: int = width - (String.length word)
        if align = 0 then
            __ret <- word + (spaces diff)
            raise Return
        if align = 2 then
            __ret <- (spaces diff) + word
            raise Return
        let mutable left = int (diff / 2)
        let mutable right = diff - left
        __ret <- ((spaces left) + word) + (spaces right)
        raise Return
        __ret
    with
        | Return -> __ret
and newFormatter (text: string) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable text = text
    try
        let mutable lines = split text "\n"
        let mutable fmtLines: string array array = [||]
        let mutable width: int array = [||]
        let mutable i: int = 0
        try
            while i < (Seq.length lines) do
                if (Seq.length (lines.[i])) = 0 then
                    i <- i + 1
                    raise Continue
                let mutable words = rstripEmpty (split (lines.[i]) "$")
                fmtLines <- Array.append fmtLines [|words|]
                let mutable j: int = 0
                while j < (Seq.length words) do
                    let wlen: int = Seq.length (words.[j])
                    if j = (Array.length width) then
                        width <- Array.append width [|wlen|]
                    else
                        if wlen > (width.[j]) then
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
and printFmt (f: Map<string, obj>) (align: int) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable f = f
    let mutable align = align
    try
        let lines = (f.["text"]) :?> string array array
        let width = (f.["width"]) :?> int array
        let mutable i: int = 0
        while i < (Seq.length lines) do
            let words = lines.[i]
            let mutable line: string = ""
            let mutable j: int = 0
            while j < (Seq.length words) do
                line <- (line + (pad (words.[j]) (width.[j]) align)) + " "
                j <- j + 1
            printfn "%s" line
            i <- i + 1
        printfn "%s" ""
        __ret
    with
        | Return -> __ret
let text: string = (((("Given$a$text$file$of$many$lines,$where$fields$within$a$line\n" + "are$delineated$by$a$single$'dollar'$character,$write$a$program\n") + "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each\n") + "column$are$separated$by$at$least$one$space.\n") + "Further,$allow$for$each$word$in$a$column$to$be$either$left\n") + "justified,$right$justified,$or$center$justified$within$its$column."
let f = newFormatter text
printFmt f 0
printFmt f 1
printFmt f 2

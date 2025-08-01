// Generated 2025-08-01 18:56 +0700

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

let small: string array = [|"zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen"|]
let tens: string array = [|""; ""; "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety"|]
let smallOrd: string array = [|"zeroth"; "first"; "second"; "third"; "fourth"; "fifth"; "sixth"; "seventh"; "eighth"; "ninth"; "tenth"; "eleventh"; "twelfth"; "thirteenth"; "fourteenth"; "fifteenth"; "sixteenth"; "seventeenth"; "eighteenth"; "nineteenth"|]
let tensOrd: string array = [|""; ""; "twentieth"; "thirtieth"; "fortieth"; "fiftieth"; "sixtieth"; "seventieth"; "eightieth"; "ninetieth"|]
let mutable words: string array = [|"Four"; "is"; "the"; "number"; "of"; "letters"; "in"; "the"; "first"; "word"; "of"; "this"; "sentence,"|]
let mutable idx: int = 0
let rec say (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n < 20 then
            __ret <- small.[n]
            raise Return
        if n < 100 then
            let mutable res: string = tens.[n / 10]
            let m: int = ((n % 10 + 10) % 10)
            if m <> 0 then
                res <- (res + "-") + (small.[m])
            __ret <- res
            raise Return
        if n < 1000 then
            let mutable res: string = (unbox<string> (say (n / 100))) + " hundred"
            let m: int = ((n % 100 + 100) % 100)
            if m <> 0 then
                res <- (res + " ") + (unbox<string> (say m))
            __ret <- res
            raise Return
        if n < 1000000 then
            let mutable res: string = (unbox<string> (say (n / 1000))) + " thousand"
            let m: int = ((n % 1000 + 1000) % 1000)
            if m <> 0 then
                res <- (res + " ") + (unbox<string> (say m))
            __ret <- res
            raise Return
        let mutable res: string = (unbox<string> (say (n / 1000000))) + " million"
        let m: int = ((n % 1000000 + 1000000) % 1000000)
        if m <> 0 then
            res <- (res + " ") + (unbox<string> (say m))
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and sayOrdinal (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n < 20 then
            __ret <- smallOrd.[n]
            raise Return
        if n < 100 then
            if (((n % 10 + 10) % 10)) = 0 then
                __ret <- tensOrd.[n / 10]
                raise Return
            __ret <- ((unbox<string> (say (n - (((n % 10 + 10) % 10))))) + "-") + (smallOrd.[((n % 10 + 10) % 10)])
            raise Return
        if n < 1000 then
            if (((n % 100 + 100) % 100)) = 0 then
                __ret <- (unbox<string> (say (n / 100))) + " hundredth"
                raise Return
            __ret <- ((unbox<string> (say (n / 100))) + " hundred ") + (unbox<string> (sayOrdinal (((n % 100 + 100) % 100))))
            raise Return
        if n < 1000000 then
            if (((n % 1000 + 1000) % 1000)) = 0 then
                __ret <- (unbox<string> (say (n / 1000))) + " thousandth"
                raise Return
            __ret <- ((unbox<string> (say (n / 1000))) + " thousand ") + (unbox<string> (sayOrdinal (((n % 1000 + 1000) % 1000))))
            raise Return
        if (((n % 1000000 + 1000000) % 1000000)) = 0 then
            __ret <- (unbox<string> (say (n / 1000000))) + " millionth"
            raise Return
        __ret <- ((unbox<string> (say (n / 1000000))) + " million ") + (unbox<string> (sayOrdinal (((n % 1000000 + 1000000) % 1000000))))
        raise Return
        __ret
    with
        | Return -> __ret
and split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable parts: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            if (((String.length sep) > 0) && ((i + (String.length sep)) <= (String.length s))) && ((_substring s i (i + (String.length sep))) = sep) then
                parts <- Array.append parts [|cur|]
                cur <- ""
                i <- i + (String.length sep)
            else
                cur <- cur + (_substring s i (i + 1))
                i <- i + 1
        parts <- Array.append parts [|cur|]
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
and countLetters (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable cnt: int = 0
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = _substring s i (i + 1)
            if ((ch >= "A") && (ch <= "Z")) || ((ch >= "a") && (ch <= "z")) then
                cnt <- cnt + 1
            i <- i + 1
        __ret <- cnt
        raise Return
        __ret
    with
        | Return -> __ret
and wordLen (w: int) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable w = w
    try
        while (Seq.length words) < w do
            idx <- idx + 1
            let n: int = countLetters (words.[idx])
            let mutable parts: string array = (say n).Split([|" "|], System.StringSplitOptions.None)
            let mutable j: int = 0
            while j < (Seq.length parts) do
                words <- Array.append words [|parts.[j]|]
                j <- j + 1
            words <- Array.append words [|"in"|]
            words <- Array.append words [|"the"|]
            parts <- unbox<string array> (((unbox<string> (sayOrdinal (idx + 1))) + ",").Split([|" "|], System.StringSplitOptions.None))
            j <- 0
            while j < (Seq.length parts) do
                words <- Array.append words [|parts.[j]|]
                j <- j + 1
        let word: string = words.[w - 1]
        __ret <- [|box (word); box (countLetters word)|]
        raise Return
        __ret
    with
        | Return -> __ret
and totalLength () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        let mutable tot: int = 0
        let mutable i: int = 0
        while i < (Seq.length words) do
            tot <- tot + (String.length (words.[i]))
            if i < ((Seq.length words) - 1) then
                tot <- tot + 1
            i <- i + 1
        __ret <- tot
        raise Return
        __ret
    with
        | Return -> __ret
and pad (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable s: string = string n
        while (String.length s) < width do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" "The lengths of the first 201 words are:"
        let mutable line: string = ""
        let mutable i: int = 1
        while i <= 201 do
            if (((i % 25 + 25) % 25)) = 1 then
                if i <> 1 then
                    printfn "%s" line
                line <- (unbox<string> (pad i 3)) + ":"
            let r: obj array = wordLen i
            let n: obj = r.[1]
            line <- (line + " ") + (unbox<string> (pad (unbox<int> n) 2))
            i <- i + 1
        printfn "%s" line
        printfn "%s" ("Length of sentence so far: " + (string (totalLength())))
        for n in [|1000; 10000; 100000; 1000000; 10000000|] do
            let r: obj array = wordLen (unbox<int> n)
            let w: obj = r.[0]
            let l: obj = r.[1]
            printfn "%s" ((((((("Word " + (unbox<string> (pad (unbox<int> n) 8))) + " is \"") + (unbox<string> w)) + "\", with ") + (string l)) + " letters.  Length of sentence so far: ") + (string (totalLength())))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

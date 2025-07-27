// Generated 2025-07-27 15:57 +0700

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

open System

let rec indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length s) do
            if (_substring s i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and fields (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable words: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = _substring s i (i + 1)
            if ((ch = " ") || (ch = "\t")) || (ch = "\n") then
                if (String.length cur) > 0 then
                    words <- unbox<string array> (Array.append words [|cur|])
                    cur <- ""
            else
                cur <- cur + ch
            i <- i + 1
        if (String.length cur) > 0 then
            words <- unbox<string array> (Array.append words [|cur|])
        __ret <- unbox<string array> words
        raise Return
        __ret
    with
        | Return -> __ret
and makePatterns () =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    try
        let digits: string array = [|"1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"|]
        let mutable pats: string array = [||]
        let mutable i: int = 0
        while i < (unbox<int> (Array.length digits)) do
            let mutable j: int = 0
            while j < (unbox<int> (Array.length digits)) do
                if j <> i then
                    let mutable k: int = 0
                    while k < (unbox<int> (Array.length digits)) do
                        if (k <> i) && (k <> j) then
                            let mutable l: int = 0
                            while l < (unbox<int> (Array.length digits)) do
                                if ((l <> i) && (l <> j)) && (l <> k) then
                                    pats <- unbox<string array> (Array.append pats [|(((digits.[i]) + (digits.[j])) + (digits.[k])) + (digits.[l])|])
                                l <- l + 1
                        k <- k + 1
                j <- j + 1
            i <- i + 1
        __ret <- unbox<string array> pats
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" ((((("Cows and bulls/player\n" + "You think of four digit number of unique digits in the range 1 to 9.\n") + "I guess.  You score my guess:\n") + "    A correct digit but not in the correct place is a cow.\n") + "    A correct digit in the correct place is a bull.\n") + "You give my score as two numbers separated with a space.")
        let mutable patterns: string array = makePatterns()
        try
            while true do
                if (unbox<int> (Array.length patterns)) = 0 then
                    printfn "%s" "Oops, check scoring."
                    __ret <- ()
                    raise Return
                let guess: string = patterns.[0]
                patterns <- unbox<string array> (Array.sub patterns 1 ((unbox<int> (Array.length patterns)) - 1))
                let mutable cows: int = 0
                let mutable bulls: int = 0
                try
                    while true do
                        printfn "%s" (("My guess: " + guess) + ".  Score? (c b) ")
                        let line: string = System.Console.ReadLine()
                        let toks: string array = fields line
                        if (unbox<int> (Array.length toks)) = 2 then
                            let c: int = int (toks.[0])
                            let b: int = int (toks.[1])
                            if ((((c >= 0) && (c <= 4)) && (b >= 0)) && (b <= 4)) && ((c + b) <= 4) then
                                cows <- c
                                bulls <- b
                                raise Break
                        printfn "%s" "Score guess as two numbers: cows bulls"
                with
                | Break -> ()
                | Continue -> ()
                if bulls = 4 then
                    printfn "%s" "I did it. :)"
                    __ret <- ()
                    raise Return
                let mutable next: string array = [||]
                let mutable idx: int = 0
                while idx < (unbox<int> (Array.length patterns)) do
                    let pat: string = patterns.[idx]
                    let mutable c: int = 0
                    let mutable b: int = 0
                    let mutable i: int = 0
                    while i < 4 do
                        let cg: string = _substring guess i (i + 1)
                        let cp: string = _substring pat i (i + 1)
                        if cg = cp then
                            b <- b + 1
                        else
                            if (unbox<int> (indexOf pat cg)) >= 0 then
                                c <- c + 1
                        i <- i + 1
                    if (c = cows) && (b = bulls) then
                        next <- unbox<string array> (Array.append next [|pat|])
                    idx <- idx + 1
                patterns <- unbox<string array> next
        with
        | Break -> ()
        | Continue -> ()
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

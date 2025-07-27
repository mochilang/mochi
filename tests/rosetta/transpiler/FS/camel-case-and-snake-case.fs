// Generated 2025-07-27 22:40 +0700

exception Break
exception Continue

exception Return

let rec trimSpace (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable start: int = 0
        while (start < (String.length s)) && ((s.Substring(start, (start + 1) - start)) = " ") do
            start <- start + 1
        let mutable ``end``: int = String.length s
        while (``end`` > start) && ((s.Substring(``end`` - 1, ``end`` - (``end`` - 1))) = " ") do
            ``end`` <- ``end`` - 1
        __ret <- s.Substring(start, ``end`` - start)
        raise Return
        __ret
    with
        | Return -> __ret
and isUpper (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        __ret <- (ch >= "A") && (ch <= "Z")
        raise Return
        __ret
    with
        | Return -> __ret
and padLeft (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable res: string = ""
        let mutable n: int = w - (String.length s)
        while n > 0 do
            res <- res + " "
            n <- n - 1
        __ret <- res + s
        raise Return
        __ret
    with
        | Return -> __ret
and snakeToCamel (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        s <- trimSpace s
        let mutable out: string = ""
        let mutable up: bool = false
        let mutable i: int = 0
        try
            while i < (String.length s) do
                let ch: string = s.Substring(i, (i + 1) - i)
                if (((ch = "_") || (ch = "-")) || (ch = " ")) || (ch = ".") then
                    up <- true
                    i <- i + 1
                    raise Continue
                if i = 0 then
                    out <- out + (unbox<string> (ch.ToLower()))
                    up <- false
                    i <- i + 1
                    raise Continue
                if up then
                    out <- out + (unbox<string> (ch.ToUpper()))
                    up <- false
                else
                    out <- out + ch
                i <- i + 1
        with
        | Break -> ()
        | Continue -> ()
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and camelToSnake (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        s <- trimSpace s
        let mutable out: string = ""
        let mutable prevUnd: bool = false
        let mutable i: int = 0
        try
            while i < (String.length s) do
                let ch: string = s.Substring(i, (i + 1) - i)
                if ((ch = " ") || (ch = "-")) || (ch = ".") then
                    if (not prevUnd) && ((String.length out) > 0) then
                        out <- out + "_"
                        prevUnd <- true
                    i <- i + 1
                    raise Continue
                if ch = "_" then
                    if (not prevUnd) && ((String.length out) > 0) then
                        out <- out + "_"
                        prevUnd <- true
                    i <- i + 1
                    raise Continue
                if isUpper ch then
                    if (i > 0) && (not prevUnd) then
                        out <- out + "_"
                    out <- out + (unbox<string> (ch.ToLower()))
                    prevUnd <- false
                else
                    out <- out + (unbox<string> (ch.ToLower()))
                    prevUnd <- false
                i <- i + 1
        with
        | Break -> ()
        | Continue -> ()
        let mutable start: int = 0
        while (start < (String.length out)) && ((out.Substring(start, (start + 1) - start)) = "_") do
            start <- start + 1
        let mutable ``end``: int = String.length out
        while (``end`` > start) && ((out.Substring(``end`` - 1, ``end`` - (``end`` - 1))) = "_") do
            ``end`` <- ``end`` - 1
        out <- out.Substring(start, ``end`` - start)
        let mutable res: string = ""
        let mutable j: int = 0
        let mutable lastUnd: bool = false
        while j < (String.length out) do
            let c: string = out.Substring(j, (j + 1) - j)
            if c = "_" then
                if not lastUnd then
                    res <- res + c
                lastUnd <- true
            else
                res <- res + c
                lastUnd <- false
            j <- j + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let samples: string array = [|"snakeCase"; "snake_case"; "snake-case"; "snake case"; "snake CASE"; "snake.case"; "variable_10_case"; "variable10Case"; "ɛrgo rE tHis"; "hurry-up-joe!"; "c://my-docs/happy_Flag-Day/12.doc"; " spaces "|]
        printfn "%s" "=== To snake_case ==="
        for s in samples do
            printfn "%s" (((unbox<string> (padLeft (unbox<string> s) 34)) + " => ") + (unbox<string> (camelToSnake (unbox<string> s))))
        printfn "%s" ""
        printfn "%s" "=== To camelCase ==="
        for s in samples do
            printfn "%s" (((unbox<string> (padLeft s 34)) + " => ") + (unbox<string> (snakeToCamel s)))
        __ret
    with
        | Return -> __ret
main()

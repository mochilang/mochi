// Generated 2025-07-25 12:29 +0700

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
open System

let rec parseIntStr (str: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable str = str
    try
        let mutable i: int = 0
        let mutable neg: bool = false
        if ((String.length str) > 0) && ((str.Substring(0, 1 - 0)) = "-") then
            neg <- true
            i <- 1
        let mutable n: int = 0
        let digits: Map<string, int> = Map.ofList [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        while i < (String.length str) do
            n <- (n * 10) + ((defaultArg (Map.tryFind (str.Substring(i, (i + 1) - i)) digits) Unchecked.defaultof<int>))
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable total: int = 0
        let mutable computer: bool = ((_now()) % 2) = 0
        printfn "%s" "Enter q to quit at any time\n"
        if computer then
            printfn "%s" "The computer will choose first"
        else
            printfn "%s" "You will choose first"
        printfn "%s" "\n\nRunning total is now 0\n\n"
        let mutable round: int = 1
        let mutable ``done``: bool = false
        try
            while not ``done`` do
                printfn "%s" (("ROUND " + (string round)) + ":\n\n")
                let mutable i: int = 0
                try
                    while (i < 2) && (not ``done``) do
                        if computer then
                            let mutable choice: int = 0
                            if total < 18 then
                                choice <- ((_now()) % 3) + 1
                            else
                                choice <- 21 - total
                            total <- total + choice
                            printfn "%s" ("The computer chooses " + (string choice))
                            printfn "%s" ("Running total is now " + (string total))
                            if total = 21 then
                                printfn "%s" "\nSo, commiserations, the computer has won!"
                                ``done`` <- true
                        else
                            try
                                while true do
                                    printfn "%s" "Your choice 1 to 3 : "
                                    let line: string = System.Console.ReadLine()
                                    if (line = "q") || (line = "Q") then
                                        printfn "%s" "OK, quitting the game"
                                        ``done`` <- true
                                        raise Break
                                    let mutable num = parseIntStr line
                                    if (num < 1) || (num > 3) then
                                        if (total + num) > 21 then
                                            printfn "%s" "Too big, try again"
                                        else
                                            printfn "%s" "Out of range, try again"
                                        raise Continue
                                    if (total + num) > 21 then
                                        printfn "%s" "Too big, try again"
                                        raise Continue
                                    total <- total + num
                                    printfn "%s" ("Running total is now " + (string total))
                                    raise Break
                            with
                            | Break -> ()
                            | Continue -> ()
                            if total = 21 then
                                printfn "%s" "\nSo, congratulations, you've won!"
                                ``done`` <- true
                        printfn "%s" "\n"
                        computer <- not computer
                        i <- i + 1
                with
                | Break -> ()
                | Continue -> ()
                round <- round + 1
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

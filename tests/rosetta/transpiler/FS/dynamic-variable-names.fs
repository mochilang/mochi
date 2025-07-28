// Generated 2025-07-28 10:03 +0700

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
            n <- int ((n * 10) + (int (digits.[(str.Substring(i, (i + 1) - i))] |> unbox<int>)))
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
        let mutable n: int = 0
        while (n < 1) || (n > 5) do
            printfn "%s" "How many integer variables do you want to create (max 5) : "
            let line: string = System.Console.ReadLine()
            if (String.length line) > 0 then
                n <- parseIntStr line
        let mutable vars: Map<string, int> = Map.ofList []
        printfn "%s" "OK, enter the variable names and their values, below\n"
        let mutable i: int = 1
        try
            while i <= n do
                try
                    printfn "%s" (("\n  Variable " + (string i)) + "\n")
                    printfn "%s" "    Name  : "
                    let name: string = System.Console.ReadLine()
                    if Map.containsKey name vars then
                        printfn "%s" "  Sorry, you've already created a variable of that name, try again"
                        raise Continue
                    let mutable value: int = 0
                    try
                        while true do
                            try
                                printfn "%s" "    Value : "
                                let valstr: string = System.Console.ReadLine()
                                if (String.length valstr) = 0 then
                                    printfn "%s" "  Not a valid integer, try again"
                                    raise Continue
                                let mutable ok: bool = true
                                let mutable j: int = 0
                                let mutable neg: bool = false
                                if (valstr.Substring(0, 1 - 0)) = "-" then
                                    neg <- true
                                    j <- 1
                                try
                                    while j < (String.length valstr) do
                                        try
                                            let ch: string = valstr.Substring(j, (j + 1) - j)
                                            if (ch < "0") || (ch > "9") then
                                                ok <- false
                                                raise Break
                                            j <- j + 1
                                        with
                                        | Continue -> ()
                                        | Break -> raise Break
                                with
                                | Break -> ()
                                | Continue -> ()
                                if not ok then
                                    printfn "%s" "  Not a valid integer, try again"
                                    raise Continue
                                value <- parseIntStr valstr
                                raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    vars <- Map.add name value vars
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        printfn "%s" "\nEnter q to quit"
        while true do
            printfn "%s" "\nWhich variable do you want to inspect : "
            let name: string = System.Console.ReadLine()
            if (unbox<string> (name.ToLower())) = "q" then
                __ret <- ()
                raise Return
            if Map.containsKey name vars then
                printfn "%s" ("It's value is " + (string (vars.[name] |> unbox<int>)))
            else
                printfn "%s" "Sorry there's no variable of that name, try again"
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

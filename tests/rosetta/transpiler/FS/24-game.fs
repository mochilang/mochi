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

let rec randDigit () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        __ret <- ((_now()) % 9) + 1
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable digits = [||]
        for i in 0 .. (4 - 1) do
            digits <- Array.append digits [|randDigit()|]
        let mutable numstr: string = ""
        for i in 0 .. (4 - 1) do
            numstr <- numstr + (string (digits.[i]))
        printfn "%s" (("Your numbers: " + numstr) + "\n")
        printfn "%s" "Enter RPN: "
        let mutable expr: string = System.Console.ReadLine()
        if (String.length expr) <> 7 then
            printfn "%s" "invalid. expression length must be 7. (4 numbers, 3 operators, no spaces)"
            __ret <- ()
            raise Return
        let mutable stack = [||]
        let mutable i: int = 0
        let mutable valid: bool = true
        try
            while i < (String.length expr) do
                let ch: string = expr.Substring(i, (i + 1) - i)
                if (ch >= "0") && (ch <= "9") then
                    if (Array.length digits) = 0 then
                        printfn "%s" "too many numbers."
                        __ret <- ()
                        raise Return
                    let mutable j: int = 0
                    while (digits.[j]) <> ((int ch) - (int "0")) do
                        j <- j + 1
                        if j = (Array.length digits) then
                            printfn "%s" "wrong numbers."
                            __ret <- ()
                            raise Return
                    digits <- Array.append (Array.sub digits 0 (j - 0)) (Array.sub digits (j + 1) ((Array.length digits) - (j + 1)))
                    stack <- Array.append stack [|float ((int ch) - (int "0"))|]
                else
                    if (Array.length stack) < 2 then
                        printfn "%s" "invalid expression syntax."
                        valid <- false
                        raise Break
                    let mutable b = stack.[(Array.length stack) - 1]
                    let mutable a = stack.[(Array.length stack) - 2]
                    if ch = "+" then
                        stack.[(Array.length stack) - 2] <- a + b
                    else
                        if ch = "-" then
                            stack.[(Array.length stack) - 2] <- a - b
                        else
                            if ch = "*" then
                                stack.[(Array.length stack) - 2] <- a * b
                            else
                                if ch = "/" then
                                    stack.[(Array.length stack) - 2] <- a / b
                                else
                                    printfn "%s" (ch + " invalid.")
                                    valid <- false
                                    raise Break
                    stack <- Array.sub stack 0 (((Array.length stack) - 1) - 0)
                i <- i + 1
        with
        | Break -> ()
        | Continue -> ()
        if valid then
            if (abs ((stack.[0]) - 24.0)) > 0.000001 then
                printfn "%s" (("incorrect. " + (string (stack.[0]))) + " != 24")
            else
                printfn "%s" "correct."
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

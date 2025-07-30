// Generated 2025-07-30 21:41 +0700

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

let rec padLeft (n: int) (width: int) =
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
and modPow (``base``: int) (exp: int) (``mod``: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    let mutable ``mod`` = ``mod``
    try
        let mutable result: int = ((1 % ``mod`` + ``mod``) % ``mod``)
        let mutable b: int = ((``base`` % ``mod`` + ``mod``) % ``mod``)
        let mutable e: int = exp
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                result <- (((result * b) % ``mod`` + ``mod``) % ``mod``)
            b <- (((b * b) % ``mod`` + ``mod``) % ``mod``)
            e <- e / 2
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable k: int = 2
        try
            while k <= 10 do
                try
                    printfn "%s" (("The first 50 Curzon numbers using a base of " + (string k)) + " :")
                    let mutable count: int = 0
                    let mutable n: int = 1
                    let mutable curzon50: int array = [||]
                    try
                        while true do
                            try
                                let d: int = (k * n) + 1
                                if (int (((((int (modPow k n d)) + 1) % d + d) % d))) = 0 then
                                    if count < 50 then
                                        curzon50 <- Array.append curzon50 [|n|]
                                    count <- count + 1
                                    if count = 50 then
                                        let mutable idx: int = 0
                                        while idx < (Seq.length curzon50) do
                                            let mutable line: string = ""
                                            let mutable j: int = 0
                                            while j < 10 do
                                                line <- (line + (unbox<string> (padLeft (curzon50.[idx]) 4))) + " "
                                                idx <- idx + 1
                                                j <- j + 1
                                            printfn "%s" (_substring line 0 ((String.length line) - 1))
                                    if count = 1000 then
                                        printfn "%s" ("\nOne thousandth: " + (string n))
                                        raise Break
                                n <- n + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    printfn "%s" ""
                    k <- k + 2
                with
                | Continue -> ()
                | Break -> raise Break
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

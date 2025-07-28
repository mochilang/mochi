// Generated 2025-07-28 10:03 +0700

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

let rec pow2 (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable p: int = 1
        let mutable i: int = 0
        while i < n do
            p <- p * 2
            i <- i + 1
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
and btoi (b: bool) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable b = b
    try
        __ret <- if b then 1 else 0
        raise Return
        __ret
    with
        | Return -> __ret
and addNoCells (cells: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable cells = cells
    try
        let mutable l: string = "O"
        let mutable r: string = "O"
        if (_substring cells 0 1) = "O" then
            l <- "."
        if (_substring cells ((String.length cells) - 1) (String.length cells)) = "O" then
            r <- "."
        cells <- (l + cells) + r
        cells <- (l + cells) + r
        __ret <- cells
        raise Return
        __ret
    with
        | Return -> __ret
and step (cells: string) (ruleVal: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable cells = cells
    let mutable ruleVal = ruleVal
    try
        let mutable newCells: string = ""
        let mutable i: int = 0
        while i < ((String.length cells) - 2) do
            let mutable bin: int = 0
            let mutable b: int = 2
            let mutable n: int = i
            while n < (i + 3) do
                bin <- bin + (int ((btoi ((_substring cells n (n + 1)) = "O")) * (pow2 b)))
                b <- b - 1
                n <- n + 1
            let mutable a: string = "."
            if (int ((((ruleVal / (int (pow2 bin))) % 2 + 2) % 2))) = 1 then
                a <- "O"
            newCells <- newCells + a
            i <- i + 1
        __ret <- newCells
        raise Return
        __ret
    with
        | Return -> __ret
and repeat (ch: string) (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    let mutable n = n
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < n do
            s <- s + ch
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and evolve (l: int) (ruleVal: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable l = l
    let mutable ruleVal = ruleVal
    try
        printfn "%s" ((" Rule #" + (string ruleVal)) + ":")
        let mutable cells: string = "O"
        let mutable x: int = 0
        while x < l do
            cells <- addNoCells cells
            let mutable width: int = 40 + ((String.length cells) / 2)
            let mutable spaces: string = repeat " " (width - (String.length cells))
            printfn "%s" (spaces + cells)
            cells <- step cells ruleVal
            x <- x + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        for r in [|90; 30|] do
            evolve 25 (int r)
            printfn "%s" ""
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

// Generated 2025-07-27 16:40 +0000

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
            if (((String.length sep) > 0) && ((i + (String.length sep)) <= (String.length s))) && ((_substring s i (i + (String.length sep))) = sep) then
                parts <- unbox<string array> (Array.append parts [|cur|])
                cur <- ""
                i <- i + (String.length sep)
            else
                cur <- cur + (s.Substring(i, (i + 1) - i))
                i <- i + 1
        parts <- unbox<string array> (Array.append parts [|cur|])
        __ret <- unbox<string array> parts
        raise Return
        __ret
    with
        | Return -> __ret
let rec join (xs: string array) (sep: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    let mutable sep = sep
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (unbox<int> (Array.length xs)) do
            if i > 0 then
                res <- res + sep
            res <- res + (unbox<string> (xs.[i]))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec repeat (ch: string) (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    let mutable n = n
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < n do
            out <- out + ch
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
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
            n <- unbox<int> ((n * 10) + (unbox<int> (digits.[(str.Substring(i, (i + 1) - i))] |> unbox<int>)))
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
let rec toBinary (n: int) (bits: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable bits = bits
    try
        let mutable b: string = ""
        let mutable ``val``: int = n
        let mutable i: int = 0
        while i < bits do
            b <- (string (((``val`` % 2 + 2) % 2))) + b
            ``val`` <- unbox<int> (``val`` / 2)
            i <- i + 1
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
let rec binToInt (bits: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable bits = bits
    try
        let mutable n: int = 0
        let mutable i: int = 0
        while i < (String.length bits) do
            n <- (n * 2) + (unbox<int> (parseIntStr (bits.Substring(i, (i + 1) - i))))
            i <- i + 1
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
let rec padRight (s: string) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable width = width
    try
        let mutable out: string = s
        while (String.length out) < width do
            out <- out + " "
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec canonicalize (cidr: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable cidr = cidr
    try
        let parts: string array = split cidr "/"
        let dotted: string = parts.[0]
        let size: int = parseIntStr (unbox<string> (parts.[1]))
        let mutable binParts: string array = [||]
        for p in split dotted "." do
            binParts <- unbox<string array> (Array.append binParts [|toBinary (parseIntStr (unbox<string> p)) 8|])
        let mutable binary: string = join binParts ""
        binary <- (binary.Substring(0, size - 0)) + (unbox<string> (repeat "0" (32 - size)))
        let mutable canonParts: string array = [||]
        let mutable i: int = 0
        while i < (String.length binary) do
            canonParts <- unbox<string array> (Array.append canonParts [|string (binToInt (binary.Substring(i, (i + 8) - i)))|])
            i <- i + 8
        __ret <- ((unbox<string> (join canonParts ".")) + "/") + (unbox<string> (parts.[1]))
        raise Return
        __ret
    with
        | Return -> __ret
let tests: string array = [|"87.70.141.1/22"; "36.18.154.103/12"; "62.62.197.11/29"; "67.137.119.181/4"; "161.214.74.21/24"; "184.232.176.184/18"|]
for t in tests do
    printfn "%s" (((unbox<string> (padRight (unbox<string> t) 18)) + " -> ") + (unbox<string> (canonicalize (unbox<string> t))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

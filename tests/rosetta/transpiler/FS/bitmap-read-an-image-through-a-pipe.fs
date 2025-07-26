// Generated 2025-07-26 12:01 +0700

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
let rec splitWs (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable parts: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = _substring s i (i + 1)
            if (((ch = " ") || (ch = "\n")) || (ch = "\t")) || (ch = "\r") then
                if (String.length cur) > 0 then
                    parts <- unbox<string array> (Array.append parts [|cur|])
                    cur <- ""
            else
                cur <- cur + ch
            i <- i + 1
        if (String.length cur) > 0 then
            parts <- unbox<string array> (Array.append parts [|cur|])
        __ret <- unbox<string array> parts
        raise Return
        __ret
    with
        | Return -> __ret
let rec parsePpm (data: string) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable data = data
    try
        let toks: string array = splitWs data
        if (int (Array.length toks)) < 4 then
            __ret <- unbox<Map<string, obj>> (Map.ofList [("err", box true)])
            raise Return
        let magic: string = toks.[0]
        let w: int = parseIntStr (unbox<string> (toks.[1]))
        let h: int = parseIntStr (unbox<string> (toks.[2]))
        let maxv: int = parseIntStr (unbox<string> (toks.[3]))
        let mutable px: int array = [||]
        let mutable i: int = 4
        while i < (int (Array.length toks)) do
            px <- unbox<int array> (Array.append px [|parseIntStr (unbox<string> (toks.[i]))|])
            i <- i + 1
        __ret <- unbox<Map<string, obj>> (Map.ofList [("magic", box magic); ("w", box w); ("h", box h); ("max", box maxv); ("px", box px)])
        raise Return
        __ret
    with
        | Return -> __ret
let ppmData: string = "P3\n2 2\n1\n0 1 1 0 1 0 0 1 1 1 0 0\n"
let img: Map<string, obj> = parsePpm ppmData
printfn "%s" ((("width=" + (string (img.["w"]))) + " height=") + (string (img.["h"])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

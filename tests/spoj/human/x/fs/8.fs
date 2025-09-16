// Generated 2025-08-27 07:05 +0700

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

let _readLine () =
    match System.Console.ReadLine() with
    | null -> ""
    | s -> s
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec _str v =
    match box v with
    | :? float as f ->
        if f = floor f then sprintf "%g.0" f else sprintf "%g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("L", "")
         .Replace("\"", "")
open System

let rec split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable parts: string array = Array.empty<string>
        let mutable cur: string = ""
        let mutable i: int64 = int64 0
        while i < (int64 (String.length (s))) do
            if (((String.length (sep)) > 0) && ((i + (int64 (String.length (sep)))) <= (int64 (String.length (s))))) && ((_substring s (int i) (int (i + (int64 (String.length (sep)))))) = sep) then
                parts <- Array.append parts [|cur|]
                cur <- ""
                i <- i + (int64 (String.length (sep)))
            else
                cur <- cur + (_substring s (int i) (int (i + (int64 1))))
                i <- i + (int64 1)
        parts <- Array.append parts [|cur|]
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
and parse_ints (line: string) =
    let mutable __ret : int64 array = Unchecked.defaultof<int64 array>
    let mutable line = line
    try
        let pieces: string array = split (line) (" ")
        let mutable nums: int64 array = Array.empty<int64>
        for p in Seq.map string (pieces) do
            if (String.length (p)) > 0 then
                nums <- Array.append nums [|(int64 (p))|]
        __ret <- nums
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let tLine: string = _readLine()
        if tLine = "" then
            __ret <- ()
            raise Return
        let t: int64 = int64 (tLine)
        let mutable caseIdx: int64 = int64 0
        while caseIdx < t do
            let header: int64 array = parse_ints (_readLine())
            let s: int64 = _idx header (int 0)
            let c: int64 = _idx header (int 1)
            let seq: int64 array = parse_ints (_readLine())
            let mutable levels: int64 array array = Array.empty<int64 array>
            levels <- Array.append levels [|seq|]
            let mutable current: int64 array = seq
            while (Seq.length (current)) > 1 do
                let mutable next: int64 array = Array.empty<int64>
                let mutable i: int64 = int64 0
                while (i + (int64 1)) < (int64 (Seq.length (current))) do
                    next <- Array.append next [|((_idx current (int (i + (int64 1)))) - (_idx current (int i)))|]
                    i <- i + (int64 1)
                levels <- Array.append levels [|next|]
                current <- next
            let mutable depth: int64 = int64 ((Seq.length (levels)) - 1)
            let mutable step: int64 = int64 0
            let mutable res: int64 array = Array.empty<int64>
            while step < c do
                let mutable bottom: int64 array = _idx levels (int depth)
                bottom <- Array.append bottom [|(_idx bottom (int ((Seq.length (bottom)) - 1)))|]
                levels.[int depth] <- bottom
                let mutable level: int64 = depth - (int64 1)
                while level >= (int64 0) do
                    let mutable arr: int64 array = _idx levels (int level)
                    let mutable arrBelow: int64 array = _idx levels (int (level + (int64 1)))
                    let mutable nextVal: int64 = (_idx arr (int ((Seq.length (arr)) - 1))) + (_idx arrBelow (int ((Seq.length (arrBelow)) - 1)))
                    arr <- Array.append arr [|nextVal|]
                    levels.[int level] <- arr
                    level <- level - (int64 1)
                res <- Array.append res [|(_idx (_idx levels (int 0)) (int ((Seq.length (_idx levels (int 0))) - 1)))|]
                step <- step + (int64 1)
            let mutable out: string = ""
            let mutable i2: int64 = int64 0
            while i2 < (int64 (Seq.length (res))) do
                if i2 > (int64 0) then
                    out <- out + " "
                out <- out + (_str (_idx res (int i2)))
                i2 <- i2 + (int64 1)
            ignore (printfn "%s" (out))
            caseIdx <- caseIdx + (int64 1)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())

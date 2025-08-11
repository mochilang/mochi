// Generated 2025-08-11 16:20 +0700

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

let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec indexOf (s: string) (sub: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable sub = sub
    try
        let n: int = String.length (s)
        let m: int = String.length (sub)
        let mutable i: int = 0
        while i <= (n - m) do
            if (_substring s i (i + m)) = sub then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and contains (s: string) (sub: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable sub = sub
    try
        __ret <- (indexOf (s) (sub)) >= 0
        raise Return
        __ret
    with
        | Return -> __ret
and validate (files: string array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable files = files
    try
        let mutable upper: string array = Array.empty<string>
        let mutable space: string array = Array.empty<string>
        let mutable hyphen: string array = Array.empty<string>
        let mutable nodir: string array = Array.empty<string>
        for f in Seq.map string (files) do
            if f <> (unbox<string> (f.ToLower())) then
                upper <- Array.append upper [|f|]
            if contains (f) (" ") then
                space <- Array.append space [|f|]
            if (contains (f) ("-")) && ((contains (f) ("/site-packages/")) = false) then
                hyphen <- Array.append hyphen [|f|]
            if not (contains (f) ("/")) then
                nodir <- Array.append nodir [|f|]
        if (Seq.length (upper)) > 0 then
            printfn "%s" ((_str (Seq.length (upper))) + " files contain uppercase characters:")
            for f in Seq.map string (upper) do
                printfn "%s" (f)
            printfn "%s" ("")
        if (Seq.length (space)) > 0 then
            printfn "%s" ((_str (Seq.length (space))) + " files contain space characters:")
            for f in Seq.map string (space) do
                printfn "%s" (f)
            printfn "%s" ("")
        if (Seq.length (hyphen)) > 0 then
            printfn "%s" ((_str (Seq.length (hyphen))) + " files contain hyphen characters:")
            for f in Seq.map string (hyphen) do
                printfn "%s" (f)
            printfn "%s" ("")
        if (Seq.length (nodir)) > 0 then
            printfn "%s" ((_str (Seq.length (nodir))) + " files are not in a directory:")
            for f in Seq.map string (nodir) do
                printfn "%s" (f)
            printfn "%s" ("")
        __ret <- (((Seq.length (upper)) + (Seq.length (space))) + (Seq.length (hyphen))) + (Seq.length (nodir))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let files: string array = unbox<string array> [|"scripts/Validate_filenames.py"; "good/file.txt"; "bad file.txt"; "/site-packages/pkg-name.py"; "nopath"; "src/hyphen-name.py"|]
        let bad: int = validate (files)
        printfn "%s" (_str (bad))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

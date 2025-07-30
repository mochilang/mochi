// Generated 2025-07-30 21:05 +0700

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

let rec endsWith (s: string) (suf: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable suf = suf
    try
        __ret <- if (String.length s) < (String.length suf) then false else ((_substring s ((String.length s) - (String.length suf)) (String.length s)) = suf)
        raise Return
        __ret
    with
        | Return -> __ret
and lastIndexOf (s: string) (sub: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable sub = sub
    try
        let mutable idx: int = 0 - 1
        let mutable i: int = 0
        while i <= ((String.length s) - (String.length sub)) do
            if (_substring s i (i + (String.length sub))) = sub then
                idx <- i
            i <- i + 1
        __ret <- idx
        raise Return
        __ret
    with
        | Return -> __ret
let extensions: string array = [|"zip"; "rar"; "7z"; "gz"; "archive"; "A##"; "tar.bz2"|]
let rec fileExtInList (filename: string) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable filename = filename
    try
        let fl: string = filename.ToLower()
        for ext in extensions do
            let ext2: string = "." + (unbox<string> (ext.ToLower()))
            if endsWith fl ext2 then
                __ret <- [|box true; box ext|]
                raise Return
        let idx: int = lastIndexOf filename "."
        if idx <> (0 - 1) then
            let t: string = _substring filename (idx + 1) (String.length filename)
            if t <> "" then
                __ret <- [|box false; box t|]
                raise Return
            __ret <- [|box false; box "<empty>"|]
            raise Return
        __ret <- [|box false; box "<none>"|]
        raise Return
        __ret
    with
        | Return -> __ret
and pad (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable t: string = s
        while (String.length t) < w do
            t <- t + " "
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" "The listed extensions are:"
        printfn "%A" extensions
        let tests: string array = [|"MyData.a##"; "MyData.tar.Gz"; "MyData.gzip"; "MyData.7z.backup"; "MyData..."; "MyData"; "MyData_v1.0.tar.bz2"; "MyData_v1.0.bz2"|]
        for t in tests do
            let res: obj array = fileExtInList t
            let ok: bool = unbox<bool> (res.[0])
            let ext: string = unbox<string> (res.[1])
            printfn "%s" ((((((unbox<string> (pad t 20)) + " => ") + (string ok)) + "  (extension = ") + ext) + ")")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

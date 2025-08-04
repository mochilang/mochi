// Generated 2025-08-04 20:44 +0700

exception Break
exception Continue

exception Return
let mutable __ret = ()

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

let FASTA: string = ((((">Rosetta_Example_1\n" + "THERECANBENOSPACE\n") + ">Rosetta_Example_2\n") + "THERECANBESEVERAL\n") + "LINESBUTTHEYALLMUST\n") + "BECONCATENATED"
let rec splitLines (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable lines: string array = [||]
        let mutable start: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = "\n" then
                lines <- Array.append lines [|_substring s start i|]
                i <- i + 1
                start <- i
            else
                i <- i + 1
        lines <- Array.append lines [|_substring s start (String.length (s))|]
        __ret <- lines
        raise Return
        __ret
    with
        | Return -> __ret
and parseFasta (text: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable text = text
    try
        let mutable key: string = ""
        let mutable ``val``: string = ""
        let mutable out: string array = [||]
        try
            for line in splitLines (text) do
                try
                    if line = "" then
                        raise Continue
                    if (_substring line 0 1) = ">" then
                        if key <> "" then
                            out <- Array.append out [|(key + ": ") + ``val``|]
                        let mutable hdr: string = _substring line 1 (String.length (line))
                        let mutable idx: int = 0
                        while (idx < (String.length (hdr))) && ((_substring hdr idx (idx + 1)) <> " ") do
                            idx <- idx + 1
                        key <- _substring hdr 0 idx
                        ``val`` <- ""
                    else
                        if key = "" then
                            printfn "%s" ("missing header")
                            __ret <- Array.empty<string>
                            raise Return
                        ``val`` <- ``val`` + line
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        if key <> "" then
            out <- Array.append out [|(key + ": ") + ``val``|]
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let res: string array = parseFasta (FASTA)
        for line in res do
            printfn "%s" (line)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

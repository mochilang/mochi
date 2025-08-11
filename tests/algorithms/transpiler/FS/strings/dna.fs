// Generated 2025-08-11 15:32 +0700

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
let rec is_valid (strand: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable strand = strand
    try
        let mutable i: int = 0
        while i < (String.length (strand)) do
            let ch: string = _substring strand i (i + 1)
            if (((ch <> "A") && (ch <> "T")) && (ch <> "C")) && (ch <> "G") then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec dna (strand: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable strand = strand
    try
        if not (is_valid (strand)) then
            printfn "%s" ("ValueError: Invalid Strand")
            __ret <- ""
            raise Return
        let mutable result: string = ""
        let mutable i: int = 0
        while i < (String.length (strand)) do
            let ch: string = _substring strand i (i + 1)
            if ch = "A" then
                result <- result + "T"
            else
                if ch = "T" then
                    result <- result + "A"
                else
                    if ch = "C" then
                        result <- result + "G"
                    else
                        result <- result + "C"
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (dna ("GCTA"))
printfn "%s" (dna ("ATGC"))
printfn "%s" (dna ("CTGA"))
printfn "%s" (dna ("GFGG"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

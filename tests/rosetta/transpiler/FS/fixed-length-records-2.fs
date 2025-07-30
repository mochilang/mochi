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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec repeat (s: string) (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable n = n
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < n do
            out <- out + s
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec trimRightSpace (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable i: int = (String.length s) - 1
        while (i >= 0) && ((s.Substring(i, (i + 1) - i)) = " ") do
            i <- i - 1
        __ret <- s.Substring(0, (i + 1) - 0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec block2text (block: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable block = block
    try
        let mutable out: string array = [||]
        for b in block do
            out <- Array.append out [|unbox<string> (trimRightSpace b)|]
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec text2block (lines: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable lines = lines
    try
        let mutable out: string array = [||]
        let mutable count: int = 0
        for line in lines do
            let mutable s: string = line
            let le: int = String.length s
            if le > 64 then
                s <- s.Substring(0, 64 - 0)
            else
                if le < 64 then
                    s <- s + (unbox<string> (repeat " " (64 - le)))
            out <- Array.append out [|s|]
            count <- count + 1
        if (((count % 16 + 16) % 16)) <> 0 then
            let pad: int = 16 - (((count % 16 + 16) % 16))
            let mutable i: int = 0
            while i < pad do
                out <- Array.append out [|unbox<string> (repeat " " 64)|]
                i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let mutable lines: string array = [|"alpha"; "beta"; "gamma"|]
let mutable blocks: string array = text2block lines
let mutable outLines: string array = block2text blocks
for l in outLines do
    if l <> "" then
        printfn "%s" l
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

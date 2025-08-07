// Generated 2025-08-07 16:27 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec repeat_char (c: string) (count: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    let mutable count = count
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < count do
            s <- s + c
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and vicsek (order: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable order = order
    try
        if order = 0 then
            __ret <- unbox<string array> [|"#"|]
            raise Return
        let prev: string array = vicsek (order - 1)
        let size: int = Seq.length (prev)
        let blank: string = repeat_char (" ") (size)
        let mutable result: string array = [||]
        let mutable i: int = 0
        while i < size do
            result <- Array.append result [|((blank + (_idx prev (i))) + blank)|]
            i <- i + 1
        i <- 0
        while i < size do
            result <- Array.append result [|(((_idx prev (i)) + (_idx prev (i))) + (_idx prev (i)))|]
            i <- i + 1
        i <- 0
        while i < size do
            result <- Array.append result [|((blank + (_idx prev (i))) + blank)|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and print_pattern (pattern: string array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable pattern = pattern
    try
        let mutable i: int = 0
        while i < (Seq.length (pattern)) do
            printfn "%s" (_idx pattern (i))
            i <- i + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let depth: int = 3
        let pattern: string array = vicsek (depth)
        print_pattern (pattern)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

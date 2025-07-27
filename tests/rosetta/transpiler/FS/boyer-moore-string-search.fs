// Generated 2025-07-27 15:57 +0700

exception Break
exception Continue

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

let rec indexOfStr (h: string) (n: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable h = h
    let mutable n = n
    try
        let hlen: int = String.length h
        let nlen: int = String.length n
        if nlen = 0 then
            __ret <- 0
            raise Return
        let mutable i: int = 0
        while i <= (hlen - nlen) do
            if (_substring h i (i + nlen)) = n then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and stringSearchSingle (h: string) (n: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable h = h
    let mutable n = n
    try
        __ret <- indexOfStr h n
        raise Return
        __ret
    with
        | Return -> __ret
and stringSearch (h: string) (n: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable h = h
    let mutable n = n
    try
        let mutable result: int array = [||]
        let mutable start: int = 0
        let hlen: int = String.length h
        let nlen: int = String.length n
        try
            while start < hlen do
                let idx: int = indexOfStr (_substring h start hlen) n
                if idx >= 0 then
                    result <- unbox<int array> (Array.append result [|start + idx|])
                    start <- (start + idx) + nlen
                else
                    raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- unbox<int array> result
        raise Return
        __ret
    with
        | Return -> __ret
and display (nums: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable nums = nums
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (unbox<int> (Array.length nums)) do
            if i > 0 then
                s <- s + ", "
            s <- s + (string (nums.[i]))
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let texts: string array = [|"GCTAGCTCTACGAGTCTA"; "GGCTATAATGCGTA"; "there would have been a time for such a word"; "needle need noodle needle"; "DKnuthusesandprogramsanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguages"; "Nearby farms grew an acre of alfalfa on the dairy's behalf, with bales of that alfalfa exchanged for milk."|]
        let patterns: string array = [|"TCTA"; "TAATAAA"; "word"; "needle"; "and"; "alfalfa"|]
        let mutable i: int = 0
        while i < (unbox<int> (Array.length texts)) do
            printfn "%s" ((("text" + (string (i + 1))) + " = ") + (unbox<string> (texts.[i])))
            i <- i + 1
        printfn "%s" ""
        let mutable j: int = 0
        while j < (unbox<int> (Array.length texts)) do
            let idxs: int array = stringSearch (unbox<string> (texts.[j])) (unbox<string> (patterns.[j]))
            printfn "%s" ((((("Found \"" + (unbox<string> (patterns.[j]))) + "\" in 'text") + (string (j + 1))) + "' at indexes ") + (unbox<string> (display idxs)))
            j <- j + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

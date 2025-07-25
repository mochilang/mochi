// Generated 2025-07-26 05:05 +0700

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
let rec amb (wordsets: string array array) (res: string array) (idx: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable wordsets = wordsets
    let mutable res = res
    let mutable idx = idx
    try
        if idx = (int (Array.length wordsets)) then
            __ret <- true
            raise Return
        let mutable prev: string = ""
        if idx > 0 then
            prev <- res.[idx - 1]
        let mutable i: int = 0
        while i < (Seq.length (wordsets.[idx])) do
            let w = (wordsets.[idx]).[i]
            if (idx = 0) || ((prev.Substring((String.length prev) - 1, (String.length prev) - ((String.length prev) - 1))) = (w.Substring(0, 1 - 0))) then
                res.[idx] <- w
                if unbox<bool> (amb wordsets res (idx + 1)) then
                    __ret <- true
                    raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let wordset: string array array = [|[|"the"; "that"; "a"|]; [|"frog"; "elephant"; "thing"|]; [|"walked"; "treaded"; "grows"|]; [|"slowly"; "quickly"|]|]
        let mutable res: string array = [||]
        let mutable i: int = 0
        while i < (int (Array.length wordset)) do
            res <- Array.append res [|""|]
            i <- i + 1
        if unbox<bool> (amb wordset res 0) then
            let mutable out: string = "[" + (unbox<string> (res.[0]))
            let mutable j: int = 1
            while j < (int (Array.length res)) do
                out <- (out + " ") + (unbox<string> (res.[j]))
                j <- j + 1
            out <- out + "]"
            printfn "%s" out
        else
            printfn "%s" "No amb found"
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

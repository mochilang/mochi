// Generated 2025-07-27 22:18 +0700

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
let rec cart2 (a: int array) (b: int array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable a = a
    let mutable b = b
    try
        let mutable p: int array array = [||]
        for x in a do
            for y in b do
                p <- unbox<int array array> (Array.append p [|[|x; y|]|])
        __ret <- unbox<int array array> p
        raise Return
        __ret
    with
        | Return -> __ret
and llStr (lst: int array array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable lst = lst
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (unbox<int> (Array.length lst)) do
            let mutable row: int array = lst.[i]
            s <- s + "["
            let mutable j: int = 0
            while j < (unbox<int> (Array.length row)) do
                s <- s + (string (row.[j]))
                if j < (unbox<int> ((unbox<int> (Array.length row)) - 1)) then
                    s <- s + " "
                j <- j + 1
            s <- s + "]"
            if i < (unbox<int> ((unbox<int> (Array.length lst)) - 1)) then
                s <- s + " "
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
        printfn "%s" (llStr (cart2 [|1; 2|] [|3; 4|]))
        printfn "%s" (llStr (cart2 [|3; 4|] [|1; 2|]))
        printfn "%s" (llStr (cart2 [|1; 2|] [||]))
        printfn "%s" (llStr (cart2 [||] [|1; 2|]))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

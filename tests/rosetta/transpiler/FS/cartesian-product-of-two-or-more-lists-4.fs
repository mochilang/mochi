// Generated 2025-07-27 23:45 +0700

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
let rec listStr (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (unbox<int> (Array.length xs)) do
            s <- s + (string (xs.[i]))
            if i < (unbox<int> ((unbox<int> (Array.length xs)) - 1)) then
                s <- s + " "
            i <- i + 1
        s <- s + "]"
        __ret <- s
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
            s <- s + (listStr (unbox<int array> (lst.[i])))
            if i < (unbox<int> ((unbox<int> (Array.length lst)) - 1)) then
                s <- s + " "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and copy (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable out: int array = [||]
        for v in xs do
            out <- unbox<int array> (Array.append out [|v|])
        __ret <- unbox<int array> out
        raise Return
        __ret
    with
        | Return -> __ret
and cartN (lists: obj) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable lists = lists
    try
        if lists = null then
            __ret <- Array.empty<int array>
            raise Return
        let a: int array array = (match lists with | :? (int array array) as a -> a | :? (obj array) as oa -> oa |> Array.map (fun v -> unbox<int array> v) | _ -> failwith "invalid cast")
        if (unbox<int> (Array.length a)) = 0 then
            __ret <- [|Array.empty<int>|]
            raise Return
        let mutable out: int array array = [||]
        let last: int = (unbox<int> (Array.length a)) - 1
        let left: int array array = cartN (Array.sub a 0 (last - 0))
        for p in left do
            for x in a.[last] do
                let mutable row: int array = copy p
                row <- unbox<int array> (Array.append row [|x|])
                out <- unbox<int array array> (Array.append out [|row|])
        __ret <- unbox<int array array> out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (llStr (cartN [|[|1; 2|]; [|3; 4|]|]))
        printfn "%s" (llStr (cartN [|[|3; 4|]; [|1; 2|]|]))
        printfn "%s" (llStr (cartN [|[|1; 2|]; [||]|]))
        printfn "%s" (llStr (cartN [|[||]; [|1; 2|]|]))
        printfn "%s" ""
        printfn "%s" "["
        for p in cartN [|[|1776; 1789|]; [|7; 12|]; [|4; 14; 23|]; [|0; 1|]|] do
            printfn "%s" (" " + (listStr p))
        printfn "%s" "]"
        printfn "%s" (llStr (cartN [|[|1; 2; 3|]; [|30|]; [|500; 100|]|]))
        printfn "%s" (llStr (cartN [|[|1; 2; 3|]; [||]; [|500; 100|]|]))
        printfn "%s" ""
        printfn "%s" (llStr (cartN null))
        printfn "%s" (llStr (cartN [||]))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

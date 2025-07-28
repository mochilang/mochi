// Generated 2025-07-28 10:03 +0700

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
open System

let rec listStr (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length xs) do
            s <- s + (string (xs.[i]))
            if i < ((Seq.length xs) - 1) then
                s <- s + " "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and ordered (xs: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    try
        if (Seq.length xs) = 0 then
            __ret <- true
            raise Return
        let mutable prev: int = xs.[0]
        let mutable i: int = 1
        while i < (Seq.length xs) do
            if (xs.[i]) < prev then
                __ret <- false
                raise Return
            prev <- xs.[i]
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and outOfOrder (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        if n < 2 then
            __ret <- Array.empty<int>
            raise Return
        let mutable r: int array = [||]
        try
            while true do
                try
                    r <- Array.empty<int>
                    let mutable i: int = 0
                    while i < n do
                        r <- Array.append r [|unbox<int> ((((_now()) % 3 + 3) % 3))|]
                        i <- i + 1
                    if not (ordered r) then
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and sort3 (a: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    try
        let mutable lo: int = 0
        let mutable mid: int = 0
        let mutable hi: int = (Seq.length a) - 1
        while mid <= hi do
            let v: int = a.[mid]
            if v = 0 then
                let tmp: int = a.[lo]
                a.[lo] <- a.[mid]
                a.[mid] <- tmp
                lo <- lo + 1
                mid <- mid + 1
            else
                if v = 1 then
                    mid <- mid + 1
                else
                    let tmp: int = a.[mid]
                    a.[mid] <- a.[hi]
                    a.[hi] <- tmp
                    hi <- hi - 1
        __ret <- a
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable f: int array = outOfOrder 12
        printfn "%s" (listStr f)
        f <- sort3 f
        printfn "%s" (listStr f)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

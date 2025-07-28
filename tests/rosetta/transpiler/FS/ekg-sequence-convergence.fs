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
let rec contains (xs: int array) (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable n = n
    try
        let mutable i: int = 0
        while i < (Seq.length xs) do
            if (xs.[i]) = n then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and gcd (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        while y <> 0 do
            let t: int = ((x % y + y) % y)
            x <- y
            y <- t
        if x < 0 then
            x <- -x
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and sortInts (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable arr: int array = xs
        let mutable n: int = Seq.length arr
        let mutable i: int = 0
        while i < n do
            let mutable j: int = 0
            while j < (n - 1) do
                if (arr.[j]) > (arr.[j + 1]) then
                    let tmp: int = arr.[j]
                    arr.[j] <- arr.[j + 1]
                    arr.[j + 1] <- tmp
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and areSame (s: int array) (t: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable t = t
    try
        if (Seq.length s) <> (Seq.length t) then
            __ret <- false
            raise Return
        let mutable a: int array = sortInts s
        let mutable b: int array = sortInts t
        let mutable i: int = 0
        while i < (Seq.length a) do
            if (a.[i]) <> (b.[i]) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and printSlice (start: int) (seq: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable start = start
    let mutable seq = seq
    try
        let mutable first: int array = [||]
        let mutable i: int = 0
        while i < 30 do
            first <- Array.append first [|seq.[i]|]
            i <- i + 1
        let mutable pad: string = ""
        if start < 10 then
            pad <- " "
        printfn "%s" (((("EKG(" + pad) + (string start)) + "): ") + (string first))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let limit: int = 100
        let starts: int array = [|2; 5; 7; 9; 10|]
        let mutable ekg: int array array = [||]
        let mutable s: int = 0
        while s < (Seq.length starts) do
            let mutable seq: int array = [|1; starts.[s]|]
            let mutable n: int = 2
            while n < limit do
                let mutable i: int = 2
                let mutable ``done``: bool = false
                while not ``done`` do
                    if (not (Seq.contains i seq)) && ((int (gcd (seq.[n - 1]) i)) > 1) then
                        seq <- Array.append seq [|i|]
                        ``done`` <- true
                    i <- i + 1
                n <- n + 1
            ekg <- Array.append ekg [|seq|]
            printSlice (starts.[s]) seq
            s <- s + 1
        let mutable i: int = 2
        let mutable found: bool = false
        try
            while i < limit do
                try
                    if (((ekg.[1]).[i]) = ((ekg.[2]).[i])) && (unbox<bool> (areSame (Array.sub ekg.[1] 0 (i - 0)) (Array.sub ekg.[2] 0 (i - 0)))) then
                        printfn "%s" ("\nEKG(5) and EKG(7) converge at term " + (string (i + 1)))
                        found <- true
                        raise Break
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        if not found then
            printfn "%s" (("\nEKG5(5) and EKG(7) do not converge within " + (string limit)) + " terms")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

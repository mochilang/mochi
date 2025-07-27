// Generated 2025-07-27 23:45 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec isPrime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        if (((n % 2 + 2) % 2)) = 0 then
            __ret <- n = 2
            raise Return
        if (((n % 3 + 3) % 3)) = 0 then
            __ret <- n = 3
            raise Return
        let mutable d: int = 5
        while (d * d) <= n do
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 2
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 4
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec bigTrim (a: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    try
        let mutable n: int = Array.length a
        while (n > 1) && ((unbox<int> (a.[n - 1])) = 0) do
            a <- unbox<int array> (Array.sub a 0 ((n - 1) - 0))
            n <- n - 1
        __ret <- unbox<int array> a
        raise Return
        __ret
    with
        | Return -> __ret
let rec bigFromInt (x: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable x = x
    try
        if x = 0 then
            __ret <- unbox<int array> [|0|]
            raise Return
        let mutable digits: int array = [||]
        let mutable n: int = x
        while n > 0 do
            digits <- unbox<int array> (Array.append digits [|((n % 10 + 10) % 10)|])
            n <- n / 10
        __ret <- unbox<int array> digits
        raise Return
        __ret
    with
        | Return -> __ret
let rec bigMulSmall (a: int array) (m: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable m = m
    try
        if m = 0 then
            __ret <- unbox<int array> [|0|]
            raise Return
        let mutable res: int array = [||]
        let mutable carry: int = 0
        let mutable i: int = 0
        while i < (unbox<int> (Array.length a)) do
            let mutable prod = (unbox<int> ((unbox<int> (a.[i])) * m)) + carry
            res <- unbox<int array> (Array.append res [|((prod % 10 + 10) % 10)|])
            carry <- unbox<int> ((unbox<int> prod) / 10)
            i <- i + 1
        while carry > 0 do
            res <- unbox<int array> (Array.append res [|((carry % 10 + 10) % 10)|])
            carry <- carry / 10
        __ret <- bigTrim res
        raise Return
        __ret
    with
        | Return -> __ret
let rec bigToString (a: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable a = a
    try
        let mutable s: string = ""
        let mutable i: int = (unbox<int> (Array.length a)) - 1
        while i >= 0 do
            s <- s + (string (a.[i]))
            i <- i - 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec pow2 (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable k = k
    try
        let mutable r: int = 1
        let mutable i: int = 0
        while i < k do
            r <- r * 2
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
let rec ccFactors (n: int) (m: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    let mutable m = m
    try
        let mutable p: int = (6 * m) + 1
        if not (isPrime p) then
            __ret <- Array.empty<int>
            raise Return
        let mutable prod: int array = bigFromInt p
        p <- (12 * m) + 1
        if not (isPrime p) then
            __ret <- Array.empty<int>
            raise Return
        prod <- bigMulSmall prod p
        let mutable i: int = 1
        while i <= (n - 2) do
            p <- (((pow2 i) * 9) * m) + 1
            if not (isPrime p) then
                __ret <- Array.empty<int>
                raise Return
            prod <- bigMulSmall prod p
            i <- i + 1
        __ret <- unbox<int array> prod
        raise Return
        __ret
    with
        | Return -> __ret
let rec ccNumbers (start: int) (``end``: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        let mutable n: int = start
        try
            while n <= ``end`` do
                let mutable m: int = 1
                if n > 4 then
                    m <- pow2 (n - 4)
                try
                    while true do
                        let num: int array = ccFactors n m
                        if (unbox<int> (Array.length num)) > 0 then
                            printfn "%s" ((("a(" + (string n)) + ") = ") + (bigToString num))
                            raise Break
                        if n <= 4 then
                            m <- m + 1
                        else
                            m <- m + (pow2 (n - 4))
                with
                | Break -> ()
                | Continue -> ()
                n <- n + 1
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
ccNumbers 3 9
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

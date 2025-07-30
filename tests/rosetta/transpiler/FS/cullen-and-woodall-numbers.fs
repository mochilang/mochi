// Generated 2025-07-30 21:41 +0700

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
let rec pow_big (``base``: bigint) (exp: int) =
    let mutable __ret : bigint = Unchecked.defaultof<bigint>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: bigint = bigint 1
        let mutable b: bigint = ``base``
        let mutable e: int = exp
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                result <- result * b
            b <- b * b
            e <- int (e / 2)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and cullen (n: int) =
    let mutable __ret : bigint = Unchecked.defaultof<bigint>
    let mutable n = n
    try
        let two_n: bigint = pow_big (bigint 2) n
        __ret <- (two_n * (bigint n)) + (bigint 1)
        raise Return
        __ret
    with
        | Return -> __ret
and woodall (n: int) =
    let mutable __ret : bigint = Unchecked.defaultof<bigint>
    let mutable n = n
    try
        __ret <- (cullen n) - (bigint 2)
        raise Return
        __ret
    with
        | Return -> __ret
and show_list (xs: bigint array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable line: string = ""
        let mutable i: int = 0
        while i < (Seq.length xs) do
            line <- line + (string (xs.[i]))
            if i < ((Seq.length xs) - 1) then
                line <- line + " "
            i <- i + 1
        __ret <- line
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable cnums: bigint array = [||]
        let mutable i: int = 1
        while i <= 20 do
            cnums <- Array.append cnums [|unbox<bigint> (cullen i)|]
            i <- i + 1
        printfn "%s" "First 20 Cullen numbers (n * 2^n + 1):"
        printfn "%s" (show_list cnums)
        let mutable wnums: bigint array = [||]
        i <- 1
        while i <= 20 do
            wnums <- Array.append wnums [|unbox<bigint> (woodall i)|]
            i <- i + 1
        printfn "%s" "\nFirst 20 Woodall numbers (n * 2^n - 1):"
        printfn "%s" (show_list wnums)
        let cprimes: bigint array = [|bigint 1; bigint 141; bigint 4713; bigint 5795; bigint 6611|]
        printfn "%s" "\nFirst 5 Cullen primes (in terms of n):"
        printfn "%s" (show_list cprimes)
        let wprimes: bigint array = [|bigint 2; bigint 3; bigint 6; bigint 30; bigint 75; bigint 81; bigint 115; bigint 123; bigint 249; bigint 362; bigint 384; bigint 462|]
        printfn "%s" "\nFirst 12 Woodall primes (in terms of n):"
        printfn "%s" (show_list wprimes)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

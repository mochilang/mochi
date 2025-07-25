// Generated 2025-07-25 14:38 +0000

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
let rec pow_int (``base``: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: int = 1
        let mutable b: int = ``base``
        let mutable e: int = exp
        while e > 0 do
            if (e % 2) = 1 then
                result <- result * b
            b <- b * b
            e <- int (e / 2)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec pow_big (``base``: bigint) (exp: int) =
    let mutable __ret : bigint = Unchecked.defaultof<bigint>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: bigint = bigint 1
        let mutable b: bigint = ``base``
        let mutable e: int = exp
        while e > 0 do
            if (e % 2) = 1 then
                result <- result * b
            b <- b * b
            e <- int (e / 2)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let mutable e1: int = pow_int 3 2
let mutable e2: int = pow_int 4 e1
let mutable ``base``: bigint = bigint 5
let mutable x: bigint = pow_big ``base`` e2
let mutable s: string = string x
printfn "%s" (String.concat " " [|sprintf "%A" "5^(4^(3^2)) has"; sprintf "%d" (String.length s); sprintf "%A" "digits:"; sprintf "%A" (s.Substring(0, 20 - 0)); sprintf "%A" "..."; sprintf "%A" (s.Substring((String.length s) - 20, (String.length s) - ((String.length s) - 20)))|])
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

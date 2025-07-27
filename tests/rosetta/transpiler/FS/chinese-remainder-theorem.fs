// Generated 2025-07-27 23:36 +0700

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
let rec egcd (a: int) (b: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        if a = 0 then
            __ret <- unbox<int array> [|b; 0; 1|]
            raise Return
        let res: int array = egcd (((b % a + a) % a)) a
        let g: int = res.[0]
        let x1: int = res.[1]
        let y1: int = res.[2]
        __ret <- unbox<int array> [|g; y1 - ((b / a) * x1); x1|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec modInv (a: int) (m: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable m = m
    try
        let r: int array = egcd a m
        if (int (r.[0])) <> 1 then
            __ret <- 0
            raise Return
        let x: int = r.[1]
        if x < 0 then
            __ret <- x + m
            raise Return
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
let rec crt (a: int array) (n: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable n = n
    try
        let mutable prod: int = 1
        let mutable i: int = 0
        while i < (int (Array.length n)) do
            prod <- int (prod * (int (n.[i])))
            i <- i + 1
        let mutable x: int = 0
        i <- 0
        while i < (int (Array.length n)) do
            let ni: int = n.[i]
            let ai: int = a.[i]
            let p: int = prod / ni
            let inv: int = modInv (((p % ni + ni) % ni)) ni
            x <- x + ((ai * inv) * p)
            i <- i + 1
        __ret <- ((x % prod + prod) % prod)
        raise Return
        __ret
    with
        | Return -> __ret
let n: int array = [|3; 5; 7|]
let a: int array = [|2; 3; 2|]
let res: int = crt a n
printfn "%s" ((string res) + " <nil>")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

// Generated 2025-07-27 22:18 +0700

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
let rec ``mod`` (n: int) (m: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    let mutable m = m
    try
        __ret <- ((((((n % m + m) % m)) + m) % m + m) % m)
        raise Return
        __ret
    with
        | Return -> __ret
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
let rec pad (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable s: string = string n
        while (String.length s) < width do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec carmichael (p1: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable p1 = p1
    try
        for h3 in 2 .. (p1 - 1) do
            try
                for d in 1 .. ((unbox<int> ((unbox<int> h3) + p1)) - 1) do
                    try
                        if ((unbox<int> (((((unbox<int> ((unbox<int> h3) + p1)) * (p1 - 1)) % d + d) % d))) = 0) && ((``mod`` ((-p1) * p1) (unbox<int> h3)) = (((d % h3 + h3) % h3))) then
                            let p2 = 1 + (unbox<int> (((p1 - 1) * (unbox<int> ((unbox<int> h3) + p1))) / d))
                            if not (isPrime (unbox<int> p2)) then
                                raise Continue
                            let p3 = 1 + (unbox<int> ((p1 * (unbox<int> p2)) / h3))
                            if not (isPrime (unbox<int> p3)) then
                                raise Continue
                            if (unbox<int> ((((p2 * p3) % (p1 - 1) + (p1 - 1)) % (p1 - 1)))) <> 1 then
                                raise Continue
                            let c = (p1 * (unbox<int> p2)) * p3
                            printfn "%s" (((((((unbox<string> (pad p1 2)) + "   ") + (unbox<string> (pad (unbox<int> p2) 4))) + "   ") + (unbox<string> (pad (unbox<int> p3) 5))) + "     ") + (string c))
                    with
                    | Break -> ()
                    | Continue -> ()
            with
            | Break -> ()
            | Continue -> ()
        __ret
    with
        | Return -> __ret
printfn "%s" "The following are Carmichael munbers for p1 <= 61:\n"
printfn "%s" "p1     p2      p3     product"
printfn "%s" "==     ==      ==     ======="
for p1 in 2 .. (62 - 1) do
    if isPrime (unbox<int> p1) then
        carmichael (unbox<int> p1)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

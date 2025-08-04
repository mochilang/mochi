// Generated 2025-08-04 20:44 +0700

exception Break
exception Continue

exception Return
let mutable __ret = ()

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let qlimit: int = 50000
let rec powf (``base``: float) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable g: float = x
        let mutable i: int = 0
        while i < 20 do
            g <- (g + (x / g)) / 2.0
            i <- i + 1
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
and modPow (``base``: int) (exp: int) (``mod``: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    let mutable ``mod`` = ``mod``
    try
        let mutable result: int = ((1 % ``mod`` + ``mod``) % ``mod``)
        let mutable b: int = ((``base`` % ``mod`` + ``mod``) % ``mod``)
        let mutable e: int = exp
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                result <- (((result * b) % ``mod`` + ``mod``) % ``mod``)
            b <- (((b * b) % ``mod`` + ``mod``) % ``mod``)
            e <- e / 2
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and mtest (m: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable m = m
    try
        if m < 4 then
            printfn "%s" ((((string (m)) + " < 4.  M") + (string (m))) + " not tested.")
            __ret <- ()
            raise Return
        let flimit: float = sqrtApprox ((powf (2.0) (m)) - 1.0)
        let mutable qlast: int = 0
        if flimit < (float qlimit) then
            qlast <- int flimit
        else
            qlast <- qlimit
        let mutable composite: bool array = [||]
        let mutable i: int = 0
        while i <= qlast do
            composite <- Array.append composite [|false|]
            i <- i + 1
        let sq: int = int (sqrtApprox (float qlast))
        let mutable q: int = 3
        try
            while true do
                try
                    if q <= sq then
                        let mutable j: int = q * q
                        while j <= qlast do
                            composite.[j] <- true
                            j <- j + q
                    let q8: int = ((q % 8 + 8) % 8)
                    if ((q8 = 1) || (q8 = 7)) && ((modPow (2) (m) (q)) = 1) then
                        printfn "%s" ((("M" + (string (m))) + " has factor ") + (string (q)))
                        __ret <- ()
                        raise Return
                    try
                        while true do
                            try
                                q <- q + 2
                                if q > qlast then
                                    printfn "%s" (("No factors of M" + (string (m))) + " found.")
                                    __ret <- ()
                                    raise Return
                                if not (_idx composite (q)) then
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        mtest (31)
        mtest (67)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

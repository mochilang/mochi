// Generated 2025-07-25 09:51 +0000

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
            if (e % 2) = 1 then
                result <- result * b
            b <- b * b
            e <- int (e / 2)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and bit_len (x: bigint) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        let mutable n: bigint = x
        let mutable c: int = 0
        while n > (bigint 0) do
            n <- n / (bigint 2)
            c <- c + 1
        __ret <- c
        raise Return
        __ret
    with
        | Return -> __ret
let mutable err: string = ""
let rec ackermann2 (m: bigint) (n: bigint) =
    let mutable __ret : bigint = Unchecked.defaultof<bigint>
    let mutable m = m
    let mutable n = n
    try
        if err <> "" then
            __ret <- bigint 0
            raise Return
        if m <= (bigint 3) then
            let mi: int = int m
            if mi = 0 then
                __ret <- n + (bigint 1)
                raise Return
            if mi = 1 then
                __ret <- n + (bigint 2)
                raise Return
            if mi = 2 then
                __ret <- ((bigint 2) * n) + (bigint 3)
                raise Return
            if mi = 3 then
                let nb: int = bit_len n
                if nb > 64 then
                    err <- ("A(m,n) had n of " + (string nb)) + " bits; too large"
                    __ret <- bigint 0
                    raise Return
                let r: bigint = pow_big (bigint 2) (int n)
                __ret <- ((bigint 8) * r) - (bigint 3)
                raise Return
        if (bit_len n) = 0 then
            __ret <- ackermann2 (m - (bigint 1)) (bigint 1)
            raise Return
        __ret <- ackermann2 (m - (bigint 1)) (ackermann2 m (n - (bigint 1)))
        raise Return
        __ret
    with
        | Return -> __ret
and show (m: int) (n: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable m = m
    let mutable n = n
    try
        err <- ""
        let res: bigint = ackermann2 (bigint m) (bigint n)
        if err <> "" then
            printfn "%s" ((((("A(" + (string m)) + ", ") + (string n)) + ") = Error: ") + err)
            __ret <- ()
            raise Return
        if (bit_len res) <= 256 then
            printfn "%s" ((((("A(" + (string m)) + ", ") + (string n)) + ") = ") + (string res))
        else
            let s: string = string res
            let pre: string = s.Substring(0, 20 - 0)
            let suf: string = s.Substring((String.length s) - 20, (String.length s) - ((String.length s) - 20))
            printfn "%s" ((((((((("A(" + (string m)) + ", ") + (string n)) + ") = ") + (string (String.length s))) + " digits starting/ending with: ") + pre) + "...") + suf)
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        show 0 0
        show 1 2
        show 2 4
        show 3 100
        show 3 1000000
        show 4 1
        show 4 2
        show 4 3
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

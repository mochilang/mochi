// Generated 2025-08-06 21:33 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec bit_and (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable ua: int = a
        let mutable ub: int = b
        let mutable res: int = 0
        let mutable bit: int = 1
        while (ua > 0) || (ub > 0) do
            if ((((ua % 2 + 2) % 2)) = 1) && ((((ub % 2 + 2) % 2)) = 1) then
                res <- res + bit
            ua <- int (ua / 2)
            ub <- int (ub / 2)
            bit <- bit * 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and count_bits_kernighan (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 0 then
            failwith ("the value of input must not be negative")
        let mutable num: int = n
        let mutable result: int = 0
        while num <> 0 do
            num <- bit_and (num) (num - 1)
            result <- result + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and count_bits_modulo (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 0 then
            failwith ("the value of input must not be negative")
        let mutable num: int = n
        let mutable result: int = 0
        while num <> 0 do
            if (((num % 2 + 2) % 2)) = 1 then
                result <- result + 1
            num <- int (num / 2)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let numbers: int array = [|25; 37; 21; 58; 0; 256|]
        let mutable i: int = 0
        while i < (Seq.length (numbers)) do
            printfn "%s" (_str (count_bits_kernighan (_idx numbers (i))))
            i <- i + 1
        i <- 0
        while i < (Seq.length (numbers)) do
            printfn "%s" (_str (count_bits_modulo (_idx numbers (i))))
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

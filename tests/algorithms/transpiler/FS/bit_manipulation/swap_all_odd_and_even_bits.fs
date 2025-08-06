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
let rec pad_left_num (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = _str (n)
        while (String.length (s)) < 5 do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and to_binary (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable sign: string = ""
        let mutable num: int = n
        if num < 0 then
            sign <- "-"
            num <- 0 - num
        let mutable bits: string = ""
        while num > 0 do
            bits <- (_str (((num % 2 + 2) % 2))) + bits
            num <- (num - (((num % 2 + 2) % 2))) / 2
        if bits = "" then
            bits <- "0"
        let min_width: int = 8
        while (String.length (bits)) < (min_width - (String.length (sign))) do
            bits <- "0" + bits
        __ret <- sign + bits
        raise Return
        __ret
    with
        | Return -> __ret
and show_bits (before: int) (after: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable before = before
    let mutable after = after
    try
        __ret <- ((((((pad_left_num (before)) + ": ") + (to_binary (before))) + "\n") + (pad_left_num (after))) + ": ") + (to_binary (after))
        raise Return
        __ret
    with
        | Return -> __ret
and lshift (num: int) (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    let mutable k = k
    try
        let mutable result: int = num
        let mutable i: int = 0
        while i < k do
            result <- result * 2
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and rshift (num: int) (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    let mutable k = k
    try
        let mutable result: int = num
        let mutable i: int = 0
        while i < k do
            result <- (result - (((result % 2 + 2) % 2))) / 2
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and swap_odd_even_bits (num: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    try
        let mutable n: obj = box (num)
        if n < 0 then
            n <- box ((int64 n) + 4294967296L)
        let mutable result: int = 0
        let mutable i: int = 0
        while i < 32 do
            let bit1: int = (((rshift (unbox<int> n) (i)) % 2 + 2) % 2)
            let bit2: int = (((rshift (unbox<int> n) (i + 1)) % 2 + 2) % 2)
            result <- (result + (lshift (bit1) (i + 1))) + (lshift (bit2) (i))
            i <- i + 2
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
        let mutable nums: int array = [|-1; 0; 1; 2; 3; 4; 23; 24|]
        let mutable i: int = 0
        while i < (Seq.length (nums)) do
            let mutable n: int = _idx nums (i)
            printfn "%s" (show_bits (n) (swap_odd_even_bits (n)))
            printfn "%s" ("")
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

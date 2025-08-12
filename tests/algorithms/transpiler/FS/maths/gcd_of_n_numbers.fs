// Generated 2025-08-12 08:17 +0700

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
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec gcd (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        while y <> 0 do
            let r: int = ((x % y + y) % y)
            x <- y
            y <- r
        if x < 0 then
            __ret <- -x
            raise Return
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and get_greatest_common_divisor (nums: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable nums = nums
    try
        if (Seq.length (nums)) = 0 then
            failwith ("at least one number is required")
        let mutable g: int = _idx nums (int 0)
        if g <= 0 then
            failwith ("numbers must be integer and greater than zero")
        let mutable i: int = 1
        while i < (Seq.length (nums)) do
            let n: int = _idx nums (int i)
            if n <= 0 then
                failwith ("numbers must be integer and greater than zero")
            g <- gcd (g) (n)
            i <- i + 1
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (get_greatest_common_divisor (unbox<int array> [|18; 45|])))
printfn "%s" (_str (get_greatest_common_divisor (unbox<int array> [|23; 37|])))
printfn "%s" (_str (get_greatest_common_divisor (unbox<int array> [|2520; 8350|])))
printfn "%s" (_str (get_greatest_common_divisor (unbox<int array> [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

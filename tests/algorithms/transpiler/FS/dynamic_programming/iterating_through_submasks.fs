// Generated 2025-08-13 07:12 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec bitwise_and (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable result: int = 0
        let mutable bit: int = 1
        let mutable x: int = a
        let mutable y: int = b
        while (x > 0) || (y > 0) do
            let abit: int = ((x % 2 + 2) % 2)
            let bbit: int = ((y % 2 + 2) % 2)
            if (abit = 1) && (bbit = 1) then
                result <- result + bit
            x <- _floordiv x 2
            y <- _floordiv y 2
            bit <- bit * 2
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and list_of_submasks (mask: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable mask = mask
    try
        if mask <= 0 then
            ignore (failwith ("mask needs to be positive integer, your input " + (_str (mask))))
        let mutable all_submasks: int array = Array.empty<int>
        let mutable submask: int = mask
        while submask <> 0 do
            all_submasks <- Array.append all_submasks [|submask|]
            submask <- bitwise_and (submask - 1) (mask)
        __ret <- all_submasks
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (list_of_submasks (15))))
ignore (printfn "%s" (_str (list_of_submasks (13))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

// Generated 2025-08-11 15:32 +0700

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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec prefix_function (s: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable s = s
    try
        let mutable pi: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (String.length (s)) do
            pi <- Array.append pi [|0|]
            i <- i + 1
        i <- 1
        while i < (String.length (s)) do
            let mutable j: int = _idx pi (int (i - 1))
            while (j > 0) && ((string (s.[i])) <> (string (s.[j]))) do
                j <- _idx pi (int (j - 1))
            if (string (s.[i])) = (string (s.[j])) then
                j <- j + 1
            pi.[int i] <- j
            i <- i + 1
        __ret <- pi
        raise Return
        __ret
    with
        | Return -> __ret
and longest_prefix (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable pi: int array = prefix_function (s)
        let mutable max_val: int = 0
        let mutable i: int = 0
        while i < (Seq.length (pi)) do
            if (_idx pi (int i)) > max_val then
                max_val <- _idx pi (int i)
            i <- i + 1
        __ret <- max_val
        raise Return
        __ret
    with
        | Return -> __ret
and list_eq_int (a: int array) (b: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        if (Seq.length (a)) <> (Seq.length (b)) then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            if (_idx a (int i)) <> (_idx b (int i)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and test_prefix_function () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let s1: string = "aabcdaabc"
        let expected1: int array = unbox<int array> [|0; 1; 0; 0; 0; 1; 2; 3; 4|]
        let r1: int array = prefix_function (s1)
        if not (list_eq_int (r1) (expected1)) then
            failwith ("prefix_function aabcdaabc failed")
        let s2: string = "asdasdad"
        let expected2: int array = unbox<int array> [|0; 0; 0; 1; 2; 3; 4; 0|]
        let r2: int array = prefix_function (s2)
        if not (list_eq_int (r2) (expected2)) then
            failwith ("prefix_function asdasdad failed")
        __ret
    with
        | Return -> __ret
and test_longest_prefix () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (longest_prefix ("aabcdaabc")) <> 4 then
            failwith ("longest_prefix example1 failed")
        if (longest_prefix ("asdasdad")) <> 4 then
            failwith ("longest_prefix example2 failed")
        if (longest_prefix ("abcab")) <> 2 then
            failwith ("longest_prefix example3 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_prefix_function()
        test_longest_prefix()
        let r1: int array = prefix_function ("aabcdaabc")
        let r2: int array = prefix_function ("asdasdad")
        printfn "%s" (_str (r1))
        printfn "%s" (_str (r2))
        printfn "%s" (_str (longest_prefix ("aabcdaabc")))
        printfn "%s" (_str (longest_prefix ("abcab")))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

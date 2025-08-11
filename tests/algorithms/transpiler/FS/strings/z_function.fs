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
let rec z_function (s: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable s = s
    try
        let mutable z: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (String.length (s)) do
            z <- Array.append z [|0|]
            i <- i + 1
        let mutable l: int = 0
        let mutable r: int = 0
        i <- 1
        while i < (String.length (s)) do
            if i <= r then
                let mutable min_edge: int = (r - i) + 1
                let zi: int = _idx z (int (i - l))
                if zi < min_edge then
                    min_edge <- zi
                z.[int i] <- min_edge
            while go_next (i) (z) (s) do
                z.[int i] <- (_idx z (int i)) + 1
            if ((i + (_idx z (int i))) - 1) > r then
                l <- i
                r <- (i + (_idx z (int i))) - 1
            i <- i + 1
        __ret <- z
        raise Return
        __ret
    with
        | Return -> __ret
and go_next (i: int) (z: int array) (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable i = i
    let mutable z = z
    let mutable s = s
    try
        __ret <- ((i + (_idx z (int i))) < (String.length (s))) && ((string (s.[_idx z (int i)])) = (string (s.[i + (_idx z (int i))])))
        raise Return
        __ret
    with
        | Return -> __ret
and find_pattern (pattern: string) (input_str: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable pattern = pattern
    let mutable input_str = input_str
    try
        let mutable answer: int = 0
        let z_res: int array = z_function (pattern + input_str)
        let mutable i: int = 0
        while i < (Seq.length (z_res)) do
            if (_idx z_res (int i)) >= (String.length (pattern)) then
                answer <- answer + 1
            i <- i + 1
        __ret <- answer
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
and test_z_function () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let s1: string = "abracadabra"
        let expected1: int array = unbox<int array> [|0; 0; 0; 1; 0; 1; 0; 4; 0; 0; 1|]
        let r1: int array = z_function (s1)
        if not (list_eq_int (r1) (expected1)) then
            failwith ("z_function abracadabra failed")
        let s2: string = "aaaa"
        let expected2: int array = unbox<int array> [|0; 3; 2; 1|]
        let r2: int array = z_function (s2)
        if not (list_eq_int (r2) (expected2)) then
            failwith ("z_function aaaa failed")
        let s3: string = "zxxzxxz"
        let expected3: int array = unbox<int array> [|0; 0; 0; 4; 0; 0; 1|]
        let r3: int array = z_function (s3)
        if not (list_eq_int (r3) (expected3)) then
            failwith ("z_function zxxzxxz failed")
        __ret
    with
        | Return -> __ret
and test_find_pattern () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (find_pattern ("abr") ("abracadabra")) <> 2 then
            failwith ("find_pattern abr failed")
        if (find_pattern ("a") ("aaaa")) <> 4 then
            failwith ("find_pattern aaaa failed")
        if (find_pattern ("xz") ("zxxzxxz")) <> 2 then
            failwith ("find_pattern xz failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_z_function()
        test_find_pattern()
        let r1: int array = z_function ("abracadabra")
        let r2: int array = z_function ("aaaa")
        let r3: int array = z_function ("zxxzxxz")
        printfn "%s" (_str (r1))
        printfn "%s" (_str (r2))
        printfn "%s" (_str (r3))
        printfn "%s" (_str (find_pattern ("abr") ("abracadabra")))
        printfn "%s" (_str (find_pattern ("a") ("aaaa")))
        printfn "%s" (_str (find_pattern ("xz") ("zxxzxxz")))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

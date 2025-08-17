// Generated 2025-08-17 08:49 +0700

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
    match box v with
    | :? float as f -> sprintf "%.15g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let rec abs_val (num: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable num = num
    try
        __ret <- if num < 0.0 then (-num) else num
        raise Return
        __ret
    with
        | Return -> __ret
and abs_min (x: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        if (Seq.length (x)) = 0 then
            ignore (failwith ("abs_min() arg is an empty sequence"))
        let mutable j: int = _idx x (int 0)
        let mutable idx: int = 0
        while idx < (Seq.length (x)) do
            let mutable i: int = _idx x (int idx)
            if (abs_val (float (float (i)))) < (abs_val (float (float (j)))) then
                j <- i
            idx <- idx + 1
        __ret <- j
        raise Return
        __ret
    with
        | Return -> __ret
and abs_max (x: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        if (Seq.length (x)) = 0 then
            ignore (failwith ("abs_max() arg is an empty sequence"))
        let mutable j: int = _idx x (int 0)
        let mutable idx: int = 0
        while idx < (Seq.length (x)) do
            let mutable i: int = _idx x (int idx)
            if (abs_val (float (float (i)))) > (abs_val (float (float (j)))) then
                j <- i
            idx <- idx + 1
        __ret <- j
        raise Return
        __ret
    with
        | Return -> __ret
and abs_max_sort (x: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        if (Seq.length (x)) = 0 then
            ignore (failwith ("abs_max_sort() arg is an empty sequence"))
        let mutable arr: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (x)) do
            arr <- Array.append arr [|(_idx x (int i))|]
            i <- i + 1
        let mutable n: int = Seq.length (arr)
        let mutable a: int = 0
        while a < n do
            let mutable b: int = 0
            while b < ((n - a) - 1) do
                if (abs_val (float (float (_idx arr (int b))))) > (abs_val (float (float (_idx arr (int (b + 1)))))) then
                    let temp: int = _idx arr (int b)
                    arr.[b] <- _idx arr (int (b + 1))
                    arr.[(b + 1)] <- temp
                b <- b + 1
            a <- a + 1
        __ret <- _idx arr (int (n - 1))
        raise Return
        __ret
    with
        | Return -> __ret
and test_abs_val () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if (abs_val (0.0)) <> 0.0 then
            ignore (failwith ("abs_val(0) failed"))
        if (abs_val (34.0)) <> 34.0 then
            ignore (failwith ("abs_val(34) failed"))
        if (abs_val (-100000000000.0)) <> 100000000000.0 then
            ignore (failwith ("abs_val large failed"))
        let mutable a: int array = unbox<int array> [|-3; -1; 2; -11|]
        if (abs_max (a)) <> (-11) then
            ignore (failwith ("abs_max failed"))
        if (abs_max_sort (a)) <> (-11) then
            ignore (failwith ("abs_max_sort failed"))
        if (abs_min (a)) <> (-1) then
            ignore (failwith ("abs_min failed"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (test_abs_val())
        ignore (printfn "%s" (_str (abs_val (-34.0))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())

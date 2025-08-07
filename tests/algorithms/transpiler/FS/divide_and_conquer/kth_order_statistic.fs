// Generated 2025-08-07 15:46 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec pivot (lst: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable lst = lst
    try
        __ret <- _idx lst (0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec kth_number (lst: int array) (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable lst = lst
    let mutable k = k
    try
        let p: int = pivot (lst)
        let mutable small: int array = [||]
        let mutable big: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            let e: int = _idx lst (i)
            if e < p then
                small <- Array.append small [|e|]
            else
                if e > p then
                    big <- Array.append big [|e|]
            i <- i + 1
        if (Seq.length (small)) = (k - 1) then
            __ret <- p
            raise Return
        else
            if (Seq.length (small)) < (k - 1) then
                __ret <- kth_number (big) ((k - (Seq.length (small))) - 1)
                raise Return
            else
                __ret <- kth_number (small) (k)
                raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (kth_number (unbox<int array> [|2; 1; 3; 4; 5|]) (3)))
printfn "%s" (_str (kth_number (unbox<int array> [|2; 1; 3; 4; 5|]) (1)))
printfn "%s" (_str (kth_number (unbox<int array> [|2; 1; 3; 4; 5|]) (5)))
printfn "%s" (_str (kth_number (unbox<int array> [|3; 2; 5; 6; 7; 8|]) (2)))
printfn "%s" (_str (kth_number (unbox<int array> [|25; 21; 98; 100; 76; 22; 43; 60; 89; 87|]) (4)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

// Generated 2025-08-11 17:23 +0700

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
let rec odd_even_sort (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable arr: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            arr <- Array.append arr [|(_idx xs (int i))|]
            i <- i + 1
        let mutable n: int = Seq.length (arr)
        let mutable sorted: bool = false
        while sorted = false do
            sorted <- true
            let mutable j: int = 0
            while j < (n - 1) do
                if (_idx arr (int j)) > (_idx arr (int (j + 1))) then
                    let tmp: int = _idx arr (int j)
                    arr.[int j] <- _idx arr (int (j + 1))
                    arr.[int (j + 1)] <- tmp
                    sorted <- false
                j <- j + 2
            j <- 1
            while j < (n - 1) do
                if (_idx arr (int j)) > (_idx arr (int (j + 1))) then
                    let tmp: int = _idx arr (int j)
                    arr.[int j] <- _idx arr (int (j + 1))
                    arr.[int (j + 1)] <- tmp
                    sorted <- false
                j <- j + 2
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and print_list (xs: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable xs = xs
    try
        let mutable i: int = 0
        let mutable out: string = ""
        while i < (Seq.length (xs)) do
            if i > 0 then
                out <- out + " "
            out <- out + (_str (_idx xs (int i)))
            i <- i + 1
        printfn "%s" (out)
        __ret
    with
        | Return -> __ret
and test_odd_even_sort () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let a: int array = unbox<int array> [|5; 4; 3; 2; 1|]
        let r1: int array = odd_even_sort (a)
        if (((((_idx r1 (int 0)) <> 1) || ((_idx r1 (int 1)) <> 2)) || ((_idx r1 (int 2)) <> 3)) || ((_idx r1 (int 3)) <> 4)) || ((_idx r1 (int 4)) <> 5) then
            failwith ("case1 failed")
        let b: int array = Array.empty<int>
        let r2: int array = odd_even_sort (b)
        if (Seq.length (r2)) <> 0 then
            failwith ("case2 failed")
        let c: int array = unbox<int array> [|-10; -1; 10; 2|]
        let r3: int array = odd_even_sort (c)
        if ((((_idx r3 (int 0)) <> (-10)) || ((_idx r3 (int 1)) <> (-1))) || ((_idx r3 (int 2)) <> 2)) || ((_idx r3 (int 3)) <> 10) then
            failwith ("case3 failed")
        let d: int array = unbox<int array> [|1; 2; 3; 4|]
        let r4: int array = odd_even_sort (d)
        if ((((_idx r4 (int 0)) <> 1) || ((_idx r4 (int 1)) <> 2)) || ((_idx r4 (int 2)) <> 3)) || ((_idx r4 (int 3)) <> 4) then
            failwith ("case4 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_odd_even_sort()
        let sample: int array = unbox<int array> [|5; 4; 3; 2; 1|]
        let mutable sorted: int array = odd_even_sort (sample)
        print_list (sorted)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

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
let rec insert_next (collection: int array) (index: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable collection = collection
    let mutable index = index
    try
        let mutable arr: int array = collection
        if (index >= (Seq.length (arr))) || ((_idx arr (int (index - 1))) <= (_idx arr (int index))) then
            __ret <- arr
            raise Return
        let mutable j: int = index - 1
        let temp: int = _idx arr (int j)
        arr.[int j] <- _idx arr (int index)
        arr.[int index] <- temp
        __ret <- insert_next (arr) (index + 1)
        raise Return
        __ret
    with
        | Return -> __ret
and rec_insertion_sort (collection: int array) (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable collection = collection
    let mutable n = n
    try
        let mutable arr: int array = collection
        if ((Seq.length (arr)) <= 1) || (n <= 1) then
            __ret <- arr
            raise Return
        arr <- insert_next (arr) (n - 1)
        __ret <- rec_insertion_sort (arr) (n - 1)
        raise Return
        __ret
    with
        | Return -> __ret
and test_rec_insertion_sort () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable col1: int array = unbox<int array> [|1; 2; 1|]
        col1 <- rec_insertion_sort (col1) (Seq.length (col1))
        if (((_idx col1 (int 0)) <> 1) || ((_idx col1 (int 1)) <> 1)) || ((_idx col1 (int 2)) <> 2) then
            failwith ("test1 failed")
        let mutable col2: int array = unbox<int array> [|2; 1; 0; -1; -2|]
        col2 <- rec_insertion_sort (col2) (Seq.length (col2))
        if (_idx col2 (int 0)) <> (0 - 2) then
            failwith ("test2 failed")
        if (_idx col2 (int 1)) <> (0 - 1) then
            failwith ("test2 failed")
        if (_idx col2 (int 2)) <> 0 then
            failwith ("test2 failed")
        if (_idx col2 (int 3)) <> 1 then
            failwith ("test2 failed")
        if (_idx col2 (int 4)) <> 2 then
            failwith ("test2 failed")
        let mutable col3: int array = unbox<int array> [|1|]
        col3 <- rec_insertion_sort (col3) (Seq.length (col3))
        if (_idx col3 (int 0)) <> 1 then
            failwith ("test3 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_rec_insertion_sort()
        let mutable numbers: int array = unbox<int array> [|5; 3; 4; 1; 2|]
        numbers <- rec_insertion_sort (numbers) (Seq.length (numbers))
        printfn "%s" (_str (numbers))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

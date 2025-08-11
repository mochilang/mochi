// Generated 2025-08-11 16:20 +0700

exception Break
exception Continue

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
let rec bubble_sort_iterative (collection: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable collection = collection
    try
        let mutable n: int = Seq.length (collection)
        try
            while n > 0 do
                try
                    let mutable swapped: bool = false
                    let mutable j: int = 0
                    while j < (n - 1) do
                        if (_idx collection (int j)) > (_idx collection (int (j + 1))) then
                            let temp: int = _idx collection (int j)
                            collection.[int j] <- _idx collection (int (j + 1))
                            collection.[int (j + 1)] <- temp
                            swapped <- true
                        j <- j + 1
                    if not swapped then
                        raise Break
                    n <- n - 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- collection
        raise Return
        __ret
    with
        | Return -> __ret
and bubble_sort_recursive (collection: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable collection = collection
    try
        let mutable n: int = Seq.length (collection)
        let mutable swapped: bool = false
        let mutable i: int = 0
        while i < (n - 1) do
            if (_idx collection (int i)) > (_idx collection (int (i + 1))) then
                let temp: int = _idx collection (int i)
                collection.[int i] <- _idx collection (int (i + 1))
                collection.[int (i + 1)] <- temp
                swapped <- true
            i <- i + 1
        if swapped then
            __ret <- bubble_sort_recursive (collection)
            raise Return
        __ret <- collection
        raise Return
        __ret
    with
        | Return -> __ret
and copy_list (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable out: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            out <- Array.append out [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and list_eq (a: int array) (b: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        if (Seq.length (a)) <> (Seq.length (b)) then
            __ret <- false
            raise Return
        let mutable k: int = 0
        while k < (Seq.length (a)) do
            if (_idx a (int k)) <> (_idx b (int k)) then
                __ret <- false
                raise Return
            k <- k + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and test_bubble_sort () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let example: int array = unbox<int array> [|0; 5; 2; 3; 2|]
        let expected: int array = unbox<int array> [|0; 2; 2; 3; 5|]
        if not (list_eq (bubble_sort_iterative (copy_list (example))) (expected)) then
            failwith ("iterative failed")
        if not (list_eq (bubble_sort_recursive (copy_list (example))) (expected)) then
            failwith ("recursive failed")
        let empty: int array = Array.empty<int>
        if (Seq.length (bubble_sort_iterative (copy_list (empty)))) <> 0 then
            failwith ("empty iterative failed")
        if (Seq.length (bubble_sort_recursive (copy_list (empty)))) <> 0 then
            failwith ("empty recursive failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_bubble_sort()
        let arr: int array = unbox<int array> [|5; 1; 4; 2; 8|]
        printfn "%s" (_str (bubble_sort_iterative (copy_list (arr))))
        printfn "%s" (_str (bubble_sort_recursive (copy_list (arr))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

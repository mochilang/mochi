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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec copy_list (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable res: int array = Array.empty<int>
        let mutable k: int = 0
        while k < (Seq.length (xs)) do
            res <- Array.append res [|(_idx xs (int k))|]
            k <- k + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and insertion_sort (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable arr: int array = copy_list (xs)
        let mutable idx: int = 1
        while idx < (Seq.length (arr)) do
            let value: int = _idx arr (int idx)
            let mutable jdx: int = idx - 1
            while (jdx >= 0) && ((_idx arr (int jdx)) > value) do
                arr.[int (jdx + 1)] <- _idx arr (int jdx)
                jdx <- jdx - 1
            arr.[int (jdx + 1)] <- value
            idx <- idx + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and merge (left: int array) (right: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable left = left
    let mutable right = right
    try
        let mutable result: int array = Array.empty<int>
        let mutable i: int = 0
        let mutable j: int = 0
        while (i < (Seq.length (left))) && (j < (Seq.length (right))) do
            if (_idx left (int i)) < (_idx right (int j)) then
                result <- Array.append result [|(_idx left (int i))|]
                i <- i + 1
            else
                result <- Array.append result [|(_idx right (int j))|]
                j <- j + 1
        while i < (Seq.length (left)) do
            result <- Array.append result [|(_idx left (int i))|]
            i <- i + 1
        while j < (Seq.length (right)) do
            result <- Array.append result [|(_idx right (int j))|]
            j <- j + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and tim_sort (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let n: int = Seq.length (xs)
        let mutable runs: int array array = Array.empty<int array>
        let mutable sorted_runs: int array array = Array.empty<int array>
        let mutable current: int array = Array.empty<int>
        current <- Array.append current [|(_idx xs (int 0))|]
        let mutable i: int = 1
        while i < n do
            if (_idx xs (int i)) < (_idx xs (int (i - 1))) then
                runs <- Array.append runs [|(copy_list (current))|]
                current <- Array.empty<int>
                current <- Array.append current [|(_idx xs (int i))|]
            else
                current <- Array.append current [|(_idx xs (int i))|]
            i <- i + 1
        runs <- Array.append runs [|(copy_list (current))|]
        let mutable r: int = 0
        while r < (Seq.length (runs)) do
            sorted_runs <- Array.append sorted_runs [|(insertion_sort (_idx runs (int r)))|]
            r <- r + 1
        let mutable result: int array = Array.empty<int>
        r <- 0
        while r < (Seq.length (sorted_runs)) do
            result <- merge (result) (_idx sorted_runs (int r))
            r <- r + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and list_to_string (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable k: int = 0
        while k < (Seq.length (xs)) do
            s <- s + (_str (_idx xs (int k)))
            if k < ((Seq.length (xs)) - 1) then
                s <- s + ", "
            k <- k + 1
        __ret <- s + "]"
        raise Return
        __ret
    with
        | Return -> __ret
let sample: int array = unbox<int array> [|5; 9; 10; 3; -4; 5; 178; 92; 46; -18; 0; 7|]
let sorted_sample: int array = tim_sort (sample)
printfn "%s" (list_to_string (sorted_sample))
let sample2: int array = unbox<int array> [|3; 2; 1|]
let sorted_sample2: int array = tim_sort (sample2)
printfn "%s" (list_to_string (sorted_sample2))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

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
let rec merge (xs: int array) (ys: int array) (reverse: bool) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable ys = ys
    let mutable reverse = reverse
    try
        let mutable result: int array = Array.empty<int>
        let mutable i: int = 0
        let mutable j: int = 0
        while (i < (Seq.length (xs))) && (j < (Seq.length (ys))) do
            if reverse then
                if (_idx xs (int i)) > (_idx ys (int j)) then
                    result <- Array.append result [|(_idx xs (int i))|]
                    i <- i + 1
                else
                    result <- Array.append result [|(_idx ys (int j))|]
                    j <- j + 1
            else
                if (_idx xs (int i)) < (_idx ys (int j)) then
                    result <- Array.append result [|(_idx xs (int i))|]
                    i <- i + 1
                else
                    result <- Array.append result [|(_idx ys (int j))|]
                    j <- j + 1
        while i < (Seq.length (xs)) do
            result <- Array.append result [|(_idx xs (int i))|]
            i <- i + 1
        while j < (Seq.length (ys)) do
            result <- Array.append result [|(_idx ys (int j))|]
            j <- j + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and strand_sort_rec (arr: int array) (reverse: bool) (solution: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    let mutable reverse = reverse
    let mutable solution = solution
    try
        if (Seq.length (arr)) = 0 then
            __ret <- solution
            raise Return
        let mutable sublist: int array = Array.empty<int>
        let mutable remaining: int array = Array.empty<int>
        sublist <- Array.append sublist [|(_idx arr (int 0))|]
        let mutable last: int = _idx arr (int 0)
        let mutable k: int = 1
        while k < (Seq.length (arr)) do
            let item: int = _idx arr (int k)
            if reverse then
                if item < last then
                    sublist <- Array.append sublist [|item|]
                    last <- item
                else
                    remaining <- Array.append remaining [|item|]
            else
                if item > last then
                    sublist <- Array.append sublist [|item|]
                    last <- item
                else
                    remaining <- Array.append remaining [|item|]
            k <- k + 1
        solution <- merge (solution) (sublist) (reverse)
        __ret <- strand_sort_rec (remaining) (reverse) (solution)
        raise Return
        __ret
    with
        | Return -> __ret
and strand_sort (arr: int array) (reverse: bool) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    let mutable reverse = reverse
    try
        __ret <- strand_sort_rec (arr) (reverse) (Array.empty<int>)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (strand_sort (unbox<int array> [|4; 3; 5; 1; 2|]) (false)))
printfn "%s" (_str (strand_sort (unbox<int array> [|4; 3; 5; 1; 2|]) (true)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

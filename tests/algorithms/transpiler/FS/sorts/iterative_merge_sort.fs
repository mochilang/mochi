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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec merge (a: int array) (low: int) (mid: int) (high: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable low = low
    let mutable mid = mid
    let mutable high = high
    try
        let mutable left: int array = Array.sub a low (mid - low)
        let mutable right: int array = Array.sub a mid ((high + 1) - mid)
        let mutable result: int array = Array.empty<int>
        while ((Seq.length (left)) > 0) && ((Seq.length (right)) > 0) do
            if (_idx left (int 0)) <= (_idx right (int 0)) then
                result <- Array.append result [|(_idx left (int 0))|]
                left <- Array.sub left 1 ((int (Array.length (left))) - 1)
            else
                result <- Array.append result [|(_idx right (int 0))|]
                right <- Array.sub right 1 ((int (Array.length (right))) - 1)
        let mutable i: int = 0
        while i < (Seq.length (left)) do
            result <- Array.append result [|(_idx left (int i))|]
            i <- i + 1
        i <- 0
        while i < (Seq.length (right)) do
            result <- Array.append result [|(_idx right (int i))|]
            i <- i + 1
        i <- 0
        while i < (Seq.length (result)) do
            a.[int (low + i)] <- _idx result (int i)
            i <- i + 1
        __ret <- a
        raise Return
        __ret
    with
        | Return -> __ret
let rec iter_merge_sort (items: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable items = items
    try
        let n: int = Seq.length (items)
        if n <= 1 then
            __ret <- items
            raise Return
        let mutable arr: int array = Array.sub items 0 ((int (Array.length (items))) - 0)
        let mutable p: int = 2
        try
            while p <= n do
                try
                    let mutable i: int = 0
                    while i < n do
                        let mutable high: int = (i + p) - 1
                        if high >= n then
                            high <- n - 1
                        let low: int = i
                        let mid: int = _floordiv ((low + high) + 1) 2
                        arr <- merge (arr) (low) (mid) (high)
                        i <- i + p
                    if ((int64 p) * (int64 2)) >= (int64 n) then
                        let mid2: int = i - p
                        arr <- merge (arr) (0) (mid2) (n - 1)
                        raise Break
                    p <- int ((int64 p) * (int64 2))
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec list_to_string (arr: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable arr = arr
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            s <- s + (_str (_idx arr (int i)))
            if i < ((Seq.length (arr)) - 1) then
                s <- s + ", "
            i <- i + 1
        __ret <- s + "]"
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (list_to_string (iter_merge_sort (unbox<int array> [|5; 9; 8; 7; 1; 2; 7|])))
printfn "%s" (list_to_string (iter_merge_sort (unbox<int array> [|1|])))
printfn "%s" (list_to_string (iter_merge_sort (unbox<int array> [|2; 1|])))
printfn "%s" (list_to_string (iter_merge_sort (unbox<int array> [|4; 3; 2; 1|])))
printfn "%s" (list_to_string (iter_merge_sort (unbox<int array> [|5; 4; 3; 2; 1|])))
printfn "%s" (list_to_string (iter_merge_sort (unbox<int array> [|-2; -9; -1; -4|])))
printfn "%s" (list_to_string (iter_merge_sort (Array.empty<int>)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

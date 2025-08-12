// Generated 2025-08-12 08:17 +0700

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let rec find_min_iterative (nums: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable nums = nums
    try
        if (Seq.length (nums)) = 0 then
            failwith ("find_min_iterative() arg is an empty sequence")
        let mutable min_num: float = _idx nums (int 0)
        let mutable i: int = 0
        while i < (Seq.length (nums)) do
            let num: float = _idx nums (int i)
            if num < min_num then
                min_num <- num
            i <- i + 1
        __ret <- min_num
        raise Return
        __ret
    with
        | Return -> __ret
and find_min_recursive (nums: float array) (left: int) (right: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable nums = nums
    let mutable left = left
    let mutable right = right
    try
        let n: int = Seq.length (nums)
        if n = 0 then
            failwith ("find_min_recursive() arg is an empty sequence")
        if (((left >= n) || (left < (0 - n))) || (right >= n)) || (right < (0 - n)) then
            failwith ("list index out of range")
        let mutable l: int = left
        let mutable r: int = right
        if l < 0 then
            l <- n + l
        if r < 0 then
            r <- n + r
        if l = r then
            __ret <- _idx nums (int l)
            raise Return
        let mid: int = _floordiv (l + r) 2
        let left_min: float = find_min_recursive (nums) (l) (mid)
        let right_min: float = find_min_recursive (nums) (mid + 1) (r)
        if left_min <= right_min then
            __ret <- left_min
            raise Return
        __ret <- right_min
        raise Return
        __ret
    with
        | Return -> __ret
and test_find_min () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let a: float array = unbox<float array> [|3.0; 2.0; 1.0|]
        if (find_min_iterative (a)) <> 1.0 then
            failwith ("iterative test1 failed")
        if (find_min_recursive (a) (0) ((Seq.length (a)) - 1)) <> 1.0 then
            failwith ("recursive test1 failed")
        let b: float array = unbox<float array> [|-3.0; -2.0; -1.0|]
        if (find_min_iterative (b)) <> (-3.0) then
            failwith ("iterative test2 failed")
        if (find_min_recursive (b) (0) ((Seq.length (b)) - 1)) <> (-3.0) then
            failwith ("recursive test2 failed")
        let c: float array = unbox<float array> [|3.0; -3.0; 0.0|]
        if (find_min_iterative (c)) <> (-3.0) then
            failwith ("iterative test3 failed")
        if (find_min_recursive (c) (0) ((Seq.length (c)) - 1)) <> (-3.0) then
            failwith ("recursive test3 failed")
        let d: float array = unbox<float array> [|1.0; 3.0; 5.0; 7.0; 9.0; 2.0; 4.0; 6.0; 8.0; 10.0|]
        if (find_min_recursive (d) (0 - (Seq.length (d))) (0 - 1)) <> 1.0 then
            failwith ("negative index test failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_find_min()
        let sample: float array = unbox<float array> [|0.0; 1.0; 2.0; 3.0; 4.0; 5.0; -3.0; 24.0; -56.0|]
        printfn "%s" (_str (find_min_iterative (sample)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

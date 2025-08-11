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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec get_bit_length (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n = 0 then
            __ret <- 1
            raise Return
        let mutable length: int = 0
        let mutable num: int = n
        while num > 0 do
            length <- length + 1
            num <- _floordiv num 2
        __ret <- length
        raise Return
        __ret
    with
        | Return -> __ret
and max_bit_length (nums: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable nums = nums
    try
        let mutable i: int = 0
        let mutable max_len: int = 0
        while i < (Seq.length (nums)) do
            let l: int = get_bit_length (_idx nums (int i))
            if l > max_len then
                max_len <- l
            i <- i + 1
        __ret <- max_len
        raise Return
        __ret
    with
        | Return -> __ret
and get_bit (num: int) (pos: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    let mutable pos = pos
    try
        let mutable n: int = num
        let mutable i: int = 0
        while i < pos do
            n <- _floordiv n 2
            i <- i + 1
        __ret <- ((n % 2 + 2) % 2)
        raise Return
        __ret
    with
        | Return -> __ret
and _msd_radix_sort (nums: int array) (bit_position: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable nums = nums
    let mutable bit_position = bit_position
    try
        if (bit_position = 0) || ((Seq.length (nums)) <= 1) then
            __ret <- nums
            raise Return
        let mutable zeros: int array = Array.empty<int>
        let mutable ones: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (nums)) do
            let mutable num: int = _idx nums (int i)
            if (get_bit (num) (bit_position - 1)) = 1 then
                ones <- Array.append ones [|num|]
            else
                zeros <- Array.append zeros [|num|]
            i <- i + 1
        zeros <- _msd_radix_sort (zeros) (bit_position - 1)
        ones <- _msd_radix_sort (ones) (bit_position - 1)
        let mutable res: int array = zeros
        i <- 0
        while i < (Seq.length (ones)) do
            res <- Array.append res [|(_idx ones (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and msd_radix_sort (nums: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable nums = nums
    try
        if (Seq.length (nums)) = 0 then
            __ret <- Array.empty<int>
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (nums)) do
            if (_idx nums (int i)) < 0 then
                failwith ("All numbers must be positive")
            i <- i + 1
        let bits: int = max_bit_length (nums)
        let result: int array = _msd_radix_sort (nums) (bits)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and msd_radix_sort_inplace (nums: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable nums = nums
    try
        __ret <- msd_radix_sort (nums)
        raise Return
        __ret
    with
        | Return -> __ret
let mutable ex1: int array = unbox<int array> [|40; 12; 1; 100; 4|]
let mutable sorted1: int array = msd_radix_sort (ex1)
printfn "%s" (_str (sorted1))
let mutable ex2: int array = Array.empty<int>
let mutable sorted2: int array = msd_radix_sort (ex2)
printfn "%s" (_str (sorted2))
let mutable ex3: int array = unbox<int array> [|123; 345; 123; 80|]
let mutable sorted3: int array = msd_radix_sort (ex3)
printfn "%s" (_str (sorted3))
let mutable ex4: int array = unbox<int array> [|1209; 834598; 1; 540402; 45|]
let mutable sorted4: int array = msd_radix_sort (ex4)
printfn "%s" (_str (sorted4))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

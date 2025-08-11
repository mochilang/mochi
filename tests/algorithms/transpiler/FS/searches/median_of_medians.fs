// Generated 2025-08-11 16:20 +0700

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
let rec set_at_int (xs: int array) (idx: int) (value: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable idx = idx
    let mutable value = value
    try
        let mutable i: int = 0
        let mutable res: int array = Array.empty<int>
        while i < (Seq.length (xs)) do
            if i = idx then
                res <- Array.append res [|value|]
            else
                res <- Array.append res [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and sort_int (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable res: int array = xs
        let mutable i: int = 1
        while i < (Seq.length (res)) do
            let key: int = _idx res (int i)
            let mutable j: int = i - 1
            while (j >= 0) && ((_idx res (int j)) > key) do
                res <- set_at_int (res) (j + 1) (_idx res (int j))
                j <- j - 1
            res <- set_at_int (res) (j + 1) (key)
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and median_of_five (arr: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    try
        let sorted: int array = sort_int (arr)
        __ret <- _idx sorted (int (_floordiv (Seq.length (sorted)) 2))
        raise Return
        __ret
    with
        | Return -> __ret
and median_of_medians (arr: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    try
        if (Seq.length (arr)) <= 5 then
            __ret <- median_of_five (arr)
            raise Return
        let mutable medians: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            if (i + 5) <= (Seq.length (arr)) then
                medians <- Array.append medians [|(median_of_five (Array.sub arr i ((i + 5) - i)))|]
            else
                medians <- Array.append medians [|(median_of_five (Array.sub arr i ((Seq.length (arr)) - i)))|]
            i <- i + 5
        __ret <- median_of_medians (medians)
        raise Return
        __ret
    with
        | Return -> __ret
and quick_select (arr: int array) (target: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable target = target
    try
        if target > (Seq.length (arr)) then
            __ret <- -1
            raise Return
        let x: int = median_of_medians (arr)
        let mutable left: int array = Array.empty<int>
        let mutable right: int array = Array.empty<int>
        let mutable check: bool = false
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            if (_idx arr (int i)) < x then
                left <- Array.append left [|(_idx arr (int i))|]
            else
                if (_idx arr (int i)) > x then
                    right <- Array.append right [|(_idx arr (int i))|]
                else
                    if (_idx arr (int i)) = x then
                        if not check then
                            check <- true
                        else
                            right <- Array.append right [|(_idx arr (int i))|]
                    else
                        right <- Array.append right [|(_idx arr (int i))|]
            i <- i + 1
        let rank_x: int = (Seq.length (left)) + 1
        let mutable answer: int = 0
        if rank_x = target then
            answer <- x
        else
            if rank_x > target then
                answer <- quick_select (left) (target)
            else
                answer <- quick_select (right) (target - rank_x)
        __ret <- answer
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (_str (median_of_five (unbox<int array> [|5; 4; 3; 2|])))
        printfn "%s" (_str (quick_select (unbox<int array> [|2; 4; 5; 7; 899; 54; 32|]) (5)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

// Generated 2025-08-12 13:41 +0700

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
let rec range_desc (start: int) (``end``: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = start
        while i >= ``end`` do
            res <- Array.append res [|i|]
            i <- i - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and range_asc (start: int) (``end``: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = start
        while i <= ``end`` do
            res <- Array.append res [|i|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and concat_lists (a: int array) (b: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: int array = a
        let mutable i: int = 0
        while i < (Seq.length (b)) do
            res <- Array.append res [|(_idx b (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and swap (xs: int array) (i: int) (j: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable i = i
    let mutable j = j
    try
        let mutable res: int array = Array.empty<int>
        let mutable k: int = 0
        while k < (Seq.length (xs)) do
            if k = i then
                res <- Array.append res [|(_idx xs (int j))|]
            else
                if k = j then
                    res <- Array.append res [|(_idx xs (int i))|]
                else
                    res <- Array.append res [|(_idx xs (int k))|]
            k <- k + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and generate_gon_ring (gon_side: int) (perm: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable gon_side = gon_side
    let mutable perm = perm
    try
        let mutable result: int array = Array.empty<int>
        result <- Array.append result [|(_idx perm (int 0))|]
        result <- Array.append result [|(_idx perm (int 1))|]
        result <- Array.append result [|(_idx perm (int 2))|]
        let mutable extended: int array = Array.append perm [|(_idx perm (int 1))|]
        let magic_number: int = if gon_side < 5 then 1 else 2
        let mutable i: int = 1
        while i < ((_floordiv (Seq.length (extended)) 3) + magic_number) do
            result <- Array.append result [|(_idx extended (int ((2 * i) + 1)))|]
            result <- Array.append result [|(_idx result (int ((3 * i) - 1)))|]
            result <- Array.append result [|(_idx extended (int ((2 * i) + 2)))|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and min_outer (numbers: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable numbers = numbers
    try
        let mutable min_val: int = _idx numbers (int 0)
        let mutable i: int = 3
        while i < (Seq.length (numbers)) do
            if (_idx numbers (int i)) < min_val then
                min_val <- _idx numbers (int i)
            i <- i + 3
        __ret <- min_val
        raise Return
        __ret
    with
        | Return -> __ret
and is_magic_gon (numbers: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable numbers = numbers
    try
        if ((((Seq.length (numbers)) % 3 + 3) % 3)) <> 0 then
            __ret <- false
            raise Return
        if (min_outer (numbers)) <> (_idx numbers (int 0)) then
            __ret <- false
            raise Return
        let total: int = ((_idx numbers (int 0)) + (_idx numbers (int 1))) + (_idx numbers (int 2))
        let mutable i: int = 3
        while i < (Seq.length (numbers)) do
            if (((_idx numbers (int i)) + (_idx numbers (int (i + 1)))) + (_idx numbers (int (i + 2)))) <> total then
                __ret <- false
                raise Return
            i <- i + 3
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and permute_search (nums: int array) (start: int) (gon_side: int) (current_max: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable nums = nums
    let mutable start = start
    let mutable gon_side = gon_side
    let mutable current_max = current_max
    try
        if start = (Seq.length (nums)) then
            let ring: int array = generate_gon_ring (gon_side) (nums)
            if is_magic_gon (ring) then
                let mutable s: string = ""
                let mutable k: int = 0
                while k < (Seq.length (ring)) do
                    s <- s + (_str (_idx ring (int k)))
                    k <- k + 1
                if s > current_max then
                    __ret <- s
                    raise Return
            __ret <- current_max
            raise Return
        let mutable res: string = current_max
        let mutable i: int = start
        while i < (Seq.length (nums)) do
            let swapped: int array = swap (nums) (start) (i)
            let candidate: string = permute_search (swapped) (start + 1) (gon_side) (res)
            if candidate > res then
                res <- candidate
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and solution (gon_side: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable gon_side = gon_side
    try
        if (gon_side < 3) || (gon_side > 5) then
            __ret <- ""
            raise Return
        let small: int array = range_desc (gon_side + 1) (1)
        let big: int array = range_asc (gon_side + 2) (gon_side * 2)
        let numbers: int array = concat_lists (small) (big)
        let max_str: string = permute_search (numbers) (0) (gon_side) ("")
        __ret <- max_str
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (solution (5)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

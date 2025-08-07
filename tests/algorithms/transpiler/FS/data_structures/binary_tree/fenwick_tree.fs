// Generated 2025-08-07 10:31 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
type FenwickTree = {
    size: int
    tree: int array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec fenwick_from_list (arr: int array) =
    let mutable __ret : FenwickTree = Unchecked.defaultof<FenwickTree>
    let mutable arr = arr
    try
        let size: int = Seq.length (arr)
        let mutable tree: int array = [||]
        let mutable i: int = 0
        while i < size do
            tree <- Array.append tree [|_idx arr (i)|]
            i <- i + 1
        i <- 1
        while i < size do
            let mutable j: int = fenwick_next (i)
            if j < size then
                tree.[j] <- (_idx tree (j)) + (_idx tree (i))
            i <- i + 1
        __ret <- { size = size; tree = tree }
        raise Return
        __ret
    with
        | Return -> __ret
let rec fenwick_empty (size: int) =
    let mutable __ret : FenwickTree = Unchecked.defaultof<FenwickTree>
    let mutable size = size
    try
        let mutable tree: int array = [||]
        let mutable i: int = 0
        while i < size do
            tree <- Array.append tree [|0|]
            i <- i + 1
        __ret <- { size = size; tree = tree }
        raise Return
        __ret
    with
        | Return -> __ret
let rec fenwick_get_array (f: FenwickTree) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable f = f
    try
        let mutable arr: int array = [||]
        let mutable i: int = 0
        while i < (f.size) do
            arr <- Array.append arr [|_idx (f.tree) (i)|]
            i <- i + 1
        i <- (f.size) - 1
        while i > 0 do
            let mutable j: int = fenwick_next (i)
            if j < (f.size) then
                arr.[j] <- (_idx arr (j)) - (_idx arr (i))
            i <- i - 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec bit_and (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable ua: int = a
        let mutable ub: int = b
        let mutable res: int = 0
        let mutable bit: int = 1
        while (ua <> 0) || (ub <> 0) do
            if ((((ua % 2 + 2) % 2)) = 1) && ((((ub % 2 + 2) % 2)) = 1) then
                res <- res + bit
            ua <- int (ua / 2)
            ub <- int (ub / 2)
            bit <- bit * 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec low_bit (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- if x = 0 then 0 else (x - (bit_and (x) (x - 1)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec fenwick_next (index: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable index = index
    try
        __ret <- index + (low_bit (index))
        raise Return
        __ret
    with
        | Return -> __ret
let rec fenwick_prev (index: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable index = index
    try
        __ret <- index - (low_bit (index))
        raise Return
        __ret
    with
        | Return -> __ret
let rec fenwick_add (f: FenwickTree) (index: int) (value: int) =
    let mutable __ret : FenwickTree = Unchecked.defaultof<FenwickTree>
    let mutable f = f
    let mutable index = index
    let mutable value = value
    try
        let mutable tree: int array = f.tree
        if index = 0 then
            tree.[0] <- (_idx tree (0)) + value
            __ret <- { size = f.size; tree = tree }
            raise Return
        let mutable i: int = index
        while i < (f.size) do
            tree.[i] <- (_idx tree (i)) + value
            i <- fenwick_next (i)
        __ret <- { size = f.size; tree = tree }
        raise Return
        __ret
    with
        | Return -> __ret
let rec fenwick_update (f: FenwickTree) (index: int) (value: int) =
    let mutable __ret : FenwickTree = Unchecked.defaultof<FenwickTree>
    let mutable f = f
    let mutable index = index
    let mutable value = value
    try
        let current: int = fenwick_get (f) (index)
        __ret <- fenwick_add (f) (index) (value - current)
        raise Return
        __ret
    with
        | Return -> __ret
let rec fenwick_prefix (f: FenwickTree) (right: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable f = f
    let mutable right = right
    try
        if right = 0 then
            __ret <- 0
            raise Return
        let mutable result: int = _idx (f.tree) (0)
        let mutable r: int = right - 1
        while r > 0 do
            result <- result + (_idx (f.tree) (r))
            r <- fenwick_prev (r)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec fenwick_query (f: FenwickTree) (left: int) (right: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable f = f
    let mutable left = left
    let mutable right = right
    try
        __ret <- (fenwick_prefix (f) (right)) - (fenwick_prefix (f) (left))
        raise Return
        __ret
    with
        | Return -> __ret
let rec fenwick_get (f: FenwickTree) (index: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable f = f
    let mutable index = index
    try
        __ret <- fenwick_query (f) (index) (index + 1)
        raise Return
        __ret
    with
        | Return -> __ret
let rec fenwick_rank_query (f: FenwickTree) (value: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable f = f
    let mutable value = value
    try
        let mutable v: int = value - (_idx (f.tree) (0))
        if v < 0 then
            __ret <- -1
            raise Return
        let mutable j: int = 1
        while (j * 2) < (f.size) do
            j <- j * 2
        let mutable i: int = 0
        let mutable jj: int = j
        while jj > 0 do
            if ((i + jj) < (f.size)) && ((_idx (f.tree) (i + jj)) <= v) then
                v <- v - (_idx (f.tree) (i + jj))
                i <- i + jj
            jj <- jj / 2
        __ret <- i
        raise Return
        __ret
    with
        | Return -> __ret
let f_base: FenwickTree = fenwick_from_list (unbox<int array> [|1; 2; 3; 4; 5|])
printfn "%s" (_repr (fenwick_get_array (f_base)))
let mutable f: FenwickTree = fenwick_from_list (unbox<int array> [|1; 2; 3; 4; 5|])
f <- fenwick_add (f) (0) (1)
f <- fenwick_add (f) (1) (2)
f <- fenwick_add (f) (2) (3)
f <- fenwick_add (f) (3) (4)
f <- fenwick_add (f) (4) (5)
printfn "%s" (_repr (fenwick_get_array (f)))
let f2: FenwickTree = fenwick_from_list (unbox<int array> [|1; 2; 3; 4; 5|])
printfn "%d" (fenwick_prefix (f2) (3))
printfn "%d" (fenwick_query (f2) (1) (4))
let f3: FenwickTree = fenwick_from_list (unbox<int array> [|1; 2; 0; 3; 0; 5|])
printfn "%d" (fenwick_rank_query (f3) (0))
printfn "%d" (fenwick_rank_query (f3) (2))
printfn "%d" (fenwick_rank_query (f3) (1))
printfn "%d" (fenwick_rank_query (f3) (3))
printfn "%d" (fenwick_rank_query (f3) (5))
printfn "%d" (fenwick_rank_query (f3) (6))
printfn "%d" (fenwick_rank_query (f3) (11))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

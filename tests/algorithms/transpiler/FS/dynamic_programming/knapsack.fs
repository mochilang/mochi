// Generated 2025-08-13 07:12 +0700

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
type KnapsackResult = {
    mutable _value: int
    mutable _subset: int array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let mutable f: int array array = Array.empty<int array>
let rec max_int (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        if a > b then
            __ret <- a
            raise Return
        else
            __ret <- b
            raise Return
        __ret
    with
        | Return -> __ret
and init_f (n: int) (w: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable n = n
    let mutable w = w
    try
        let mutable table: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i <= n do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j <= w do
                if (i = 0) || (j = 0) then
                    row <- Array.append row [|0|]
                else
                    row <- Array.append row [|(-1)|]
                j <- j + 1
            table <- Array.append table [|row|]
            i <- i + 1
        __ret <- table
        raise Return
        __ret
    with
        | Return -> __ret
and mf_knapsack (i: int) (wt: int array) (``val``: int array) (j: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    let mutable wt = wt
    let mutable ``val`` = ``val``
    let mutable j = j
    try
        if (_idx (_idx f (int i)) (int j)) < 0 then
            if j < (_idx wt (int (i - 1))) then
                f.[i].[j] <- mf_knapsack (i - 1) (wt) (``val``) (j)
            else
                let without_item: int = mf_knapsack (i - 1) (wt) (``val``) (j)
                let with_item: int = (mf_knapsack (i - 1) (wt) (``val``) (j - (_idx wt (int (i - 1))))) + (_idx ``val`` (int (i - 1)))
                f.[i].[j] <- max_int (without_item) (with_item)
        __ret <- _idx (_idx f (int i)) (int j)
        raise Return
        __ret
    with
        | Return -> __ret
and create_matrix (rows: int) (cols: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable rows = rows
    let mutable cols = cols
    try
        let mutable matrix: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i <= rows do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j <= cols do
                row <- Array.append row [|0|]
                j <- j + 1
            matrix <- Array.append matrix [|row|]
            i <- i + 1
        __ret <- matrix
        raise Return
        __ret
    with
        | Return -> __ret
and knapsack (w: int) (wt: int array) (``val``: int array) (n: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable w = w
    let mutable wt = wt
    let mutable ``val`` = ``val``
    let mutable n = n
    try
        let mutable dp: int array array = create_matrix (n) (w)
        let mutable i: int = 1
        while i <= n do
            let mutable w_: int = 1
            while w_ <= w do
                if (_idx wt (int (i - 1))) <= w_ then
                    let include: int = (_idx ``val`` (int (i - 1))) + (_idx (_idx dp (int (i - 1))) (int (w_ - (_idx wt (int (i - 1))))))
                    let exclude: int = _idx (_idx dp (int (i - 1))) (int w_)
                    dp.[i].[w_] <- max_int (include) (exclude)
                else
                    dp.[i].[w_] <- _idx (_idx dp (int (i - 1))) (int w_)
                w_ <- w_ + 1
            i <- i + 1
        __ret <- dp
        raise Return
        __ret
    with
        | Return -> __ret
and construct_solution (dp: int array array) (wt: int array) (i: int) (j: int) (optimal_set: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable dp = dp
    let mutable wt = wt
    let mutable i = i
    let mutable j = j
    let mutable optimal_set = optimal_set
    try
        if (i > 0) && (j > 0) then
            if (_idx (_idx dp (int (i - 1))) (int j)) = (_idx (_idx dp (int i)) (int j)) then
                __ret <- construct_solution (dp) (wt) (i - 1) (j) (optimal_set)
                raise Return
            else
                let with_prev: int array = construct_solution (dp) (wt) (i - 1) (j - (_idx wt (int (i - 1)))) (optimal_set)
                __ret <- Array.append with_prev [|i|]
                raise Return
        __ret <- optimal_set
        raise Return
        __ret
    with
        | Return -> __ret
and knapsack_with_example_solution (w: int) (wt: int array) (``val``: int array) =
    let mutable __ret : KnapsackResult = Unchecked.defaultof<KnapsackResult>
    let mutable w = w
    let mutable wt = wt
    let mutable ``val`` = ``val``
    try
        let num_items: int = Seq.length (wt)
        let dp_table: int array array = knapsack (w) (wt) (``val``) (num_items)
        let optimal_val: int = _idx (_idx dp_table (int num_items)) (int w)
        let _subset: int array = construct_solution (dp_table) (wt) (num_items) (w) (Array.empty<int>)
        __ret <- { _value = optimal_val; _subset = _subset }
        raise Return
        __ret
    with
        | Return -> __ret
and format_set (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable res: string = "{"
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            res <- res + (_str (_idx xs (int i)))
            if (i + 1) < (Seq.length (xs)) then
                res <- res + ", "
            i <- i + 1
        res <- res + "}"
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let val_list: int array = unbox<int array> [|3; 2; 4; 4|]
let wt_list: int array = unbox<int array> [|4; 3; 2; 3|]
let n: int = 4
let w_cap: int = 6
f <- init_f (n) (w_cap)
let dp_table: int array array = knapsack (w_cap) (wt_list) (val_list) (n)
let optimal_solution: int = _idx (_idx dp_table (int n)) (int w_cap)
ignore (printfn "%d" (optimal_solution))
ignore (printfn "%d" (mf_knapsack (n) (wt_list) (val_list) (w_cap)))
let example: KnapsackResult = knapsack_with_example_solution (w_cap) (wt_list) (val_list)
ignore (printfn "%s" ("optimal_value = " + (_str (example._value))))
ignore (printfn "%s" ("An optimal subset corresponding to the optimal value " + (format_set (example._subset))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

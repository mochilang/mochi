// Generated 2025-08-08 16:34 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Itemset = {
    mutable _items: string array
    mutable _support: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec load_data () =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    try
        __ret <- [|[|"milk"|]; [|"milk"; "butter"|]; [|"milk"; "bread"|]; [|"milk"; "bread"; "chips"|]|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains_string (xs: string array) (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable s = s
    try
        for v in Seq.map string (xs) do
            if v = s then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_subset (candidate: string array) (transaction: string array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable candidate = candidate
    let mutable transaction = transaction
    try
        for it in Seq.map string (candidate) do
            if not (contains_string (transaction) (it)) then
                __ret <- false
                raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec lists_equal (a: string array) (b: string array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        if (Seq.length (a)) <> (Seq.length (b)) then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            if (_idx a (i)) <> (_idx b (i)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains_list (itemset: string array array) (item: string array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable itemset = itemset
    let mutable item = item
    try
        for l in itemset do
            if lists_equal (l) (item) then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec count_list (itemset: string array array) (item: string array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable itemset = itemset
    let mutable item = item
    try
        let mutable c: int = 0
        for l in itemset do
            if lists_equal (l) (item) then
                c <- c + 1
        __ret <- c
        raise Return
        __ret
    with
        | Return -> __ret
let rec slice_list (xs: string array array) (start: int) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable xs = xs
    let mutable start = start
    try
        let mutable res: string array array = Array.empty<string array>
        let mutable i: int = start
        while i < (Seq.length (xs)) do
            res <- Array.append res [|(_idx xs (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec combinations_lists (xs: string array array) (k: int) =
    let mutable __ret : string array array array = Unchecked.defaultof<string array array array>
    let mutable xs = xs
    let mutable k = k
    try
        let mutable result: string array array array = Array.empty<string array array>
        if k = 0 then
            result <- Array.append result [|[||]|]
            __ret <- result
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            let head: string array = _idx xs (i)
            let tail: string array array = slice_list (xs) (i + 1)
            let tail_combos: string array array array = combinations_lists (tail) (k - 1)
            for combo in tail_combos do
                let mutable new_combo: string array array = Array.empty<string array>
                new_combo <- Array.append new_combo [|head|]
                for c in combo do
                    new_combo <- Array.append new_combo [|c|]
                result <- Array.append result [|new_combo|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec prune (itemset: string array array) (candidates: string array array array) (length: int) =
    let mutable __ret : string array array array = Unchecked.defaultof<string array array array>
    let mutable itemset = itemset
    let mutable candidates = candidates
    let mutable length = length
    try
        let mutable pruned: string array array array = Array.empty<string array array>
        try
            for candidate in candidates do
                try
                    let mutable is_subsequence: bool = true
                    try
                        for item in candidate do
                            try
                                if (not (contains_list (itemset) (item))) || ((count_list (itemset) (item)) < (length - 1)) then
                                    is_subsequence <- false
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if is_subsequence then
                        pruned <- Array.append pruned [|candidate|]
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- pruned
        raise Return
        __ret
    with
        | Return -> __ret
let rec sort_strings (xs: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    try
        let mutable res: string array = Array.empty<string>
        for s in Seq.map string (xs) do
            res <- Array.append res [|s|]
        let mutable i: int = 0
        while i < (Seq.length (res)) do
            let mutable j: int = i + 1
            while j < (Seq.length (res)) do
                if (_idx res (j)) < (_idx res (i)) then
                    let tmp: string = _idx res (i)
                    res.[i] <- _idx res (j)
                    res.[j] <- tmp
                j <- j + 1
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec itemset_to_string (xs: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if i > 0 then
                s <- s + ", "
            s <- ((s + "'") + (_idx xs (i))) + "'"
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec apriori (data: string array array) (min_support: int) =
    let mutable __ret : Itemset array = Unchecked.defaultof<Itemset array>
    let mutable data = data
    let mutable min_support = min_support
    try
        let mutable itemset: obj = box (Array.empty<string array>)
        for transaction in data do
            let mutable t: string array = Array.empty<string>
            for v in Seq.map string (transaction) do
                t <- Array.append t [|v|]
            itemset <- Array.append itemset [|t|]
        let mutable frequent: Itemset array = Array.empty<Itemset>
        let mutable length: int = 1
        while (Seq.length (itemset)) > 0 do
            let mutable counts: int array = Array.empty<int>
            let mutable idx: int = 0
            while idx < (Seq.length (itemset)) do
                counts <- Array.append counts [|0|]
                idx <- idx + 1
            for transaction in data do
                let mutable j: int = 0
                while j < (Seq.length (itemset)) do
                    let candidate: string array = _idx itemset (j)
                    if is_subset (candidate) (transaction) then
                        counts.[j] <- (_idx counts (j)) + 1
                    j <- j + 1
            let mutable new_itemset: string array array = Array.empty<string array>
            let mutable k: int = 0
            while k < (Seq.length (itemset)) do
                if (_idx counts (k)) >= min_support then
                    new_itemset <- Array.append new_itemset [|(_idx itemset (k))|]
                k <- k + 1
            itemset <- new_itemset
            let mutable m: int = 0
            while m < (Seq.length (itemset)) do
                let mutable sorted_item: string array = sort_strings (_idx itemset (m))
                frequent <- Array.append frequent [|{ _items = sorted_item; _support = _idx counts (m) }|]
                m <- m + 1
            length <- length + 1
            let combos: string array array array = combinations_lists (itemset) (length)
            itemset <- box (prune (itemset) (combos) (length))
        __ret <- frequent
        raise Return
        __ret
    with
        | Return -> __ret
let mutable frequent_itemsets: Itemset array = apriori (load_data()) (2)
for fi in frequent_itemsets do
    printfn "%s" (((itemset_to_string (fi._items)) + ": ") + (_str (fi._support)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

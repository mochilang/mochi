// Generated 2025-08-11 17:23 +0700

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
let rec binary_search_insertion_from (sorted_list: int array) (item: int) (start: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable sorted_list = sorted_list
    let mutable item = item
    let mutable start = start
    try
        let mutable left: int = start
        let mutable right: int = (Seq.length (sorted_list)) - 1
        try
            while left <= right do
                try
                    let middle: int = _floordiv (left + right) 2
                    if left = right then
                        if (_idx sorted_list (int middle)) < item then
                            left <- middle + 1
                        raise Break
                    else
                        if (_idx sorted_list (int middle)) < item then
                            left <- middle + 1
                        else
                            right <- middle - 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable result: int array = Array.empty<int>
        let mutable i: int = 0
        while i < left do
            result <- Array.append result [|(_idx sorted_list (int i))|]
            i <- i + 1
        result <- Array.append result [|item|]
        while i < (Seq.length (sorted_list)) do
            result <- Array.append result [|(_idx sorted_list (int i))|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and binary_search_insertion (sorted_list: int array) (item: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable sorted_list = sorted_list
    let mutable item = item
    try
        __ret <- binary_search_insertion_from (sorted_list) (item) (0)
        raise Return
        __ret
    with
        | Return -> __ret
and merge (left: int array array) (right: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable left = left
    let mutable right = right
    try
        let mutable result: int array array = Array.empty<int array>
        let mutable i: int = 0
        let mutable j: int = 0
        while (i < (Seq.length (left))) && (j < (Seq.length (right))) do
            if (_idx (_idx left (int i)) (int 0)) < (_idx (_idx right (int j)) (int 0)) then
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
and sortlist_2d (list_2d: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable list_2d = list_2d
    try
        let length: int = Seq.length (list_2d)
        if length <= 1 then
            __ret <- list_2d
            raise Return
        let middle: int = _floordiv length 2
        let mutable left: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < middle do
            left <- Array.append left [|(_idx list_2d (int i))|]
            i <- i + 1
        let mutable right: int array array = Array.empty<int array>
        let mutable j: int = middle
        while j < length do
            right <- Array.append right [|(_idx list_2d (int j))|]
            j <- j + 1
        __ret <- merge (sortlist_2d (left)) (sortlist_2d (right))
        raise Return
        __ret
    with
        | Return -> __ret
and merge_insertion_sort (collection: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable collection = collection
    try
        if (Seq.length (collection)) <= 1 then
            __ret <- collection
            raise Return
        let mutable two_paired_list: int array array = Array.empty<int array>
        let mutable has_last_odd_item: bool = false
        let mutable i: int = 0
        while i < (Seq.length (collection)) do
            if i = ((Seq.length (collection)) - 1) then
                has_last_odd_item <- true
            else
                let a: int = _idx collection (int i)
                let b: int = _idx collection (int (i + 1))
                if a < b then
                    two_paired_list <- Array.append two_paired_list [|[|a; b|]|]
                else
                    two_paired_list <- Array.append two_paired_list [|[|b; a|]|]
            i <- i + 2
        let mutable sorted_list_2d: int array array = sortlist_2d (two_paired_list)
        let mutable result: int array = Array.empty<int>
        i <- 0
        while i < (Seq.length (sorted_list_2d)) do
            result <- Array.append result [|(_idx (_idx sorted_list_2d (int i)) (int 0))|]
            i <- i + 1
        result <- Array.append result [|(_idx (_idx sorted_list_2d (int ((Seq.length (sorted_list_2d)) - 1))) (int 1))|]
        if has_last_odd_item then
            result <- binary_search_insertion (result) (_idx collection (int ((Seq.length (collection)) - 1)))
        let mutable inserted_before: bool = false
        let mutable idx: int = 0
        while idx < ((Seq.length (sorted_list_2d)) - 1) do
            if has_last_odd_item && ((_idx result (int idx)) = (_idx collection (int ((Seq.length (collection)) - 1)))) then
                inserted_before <- true
            let pivot: int = _idx (_idx sorted_list_2d (int idx)) (int 1)
            if inserted_before then
                result <- binary_search_insertion_from (result) (pivot) (idx + 2)
            else
                result <- binary_search_insertion_from (result) (pivot) (idx + 1)
            idx <- idx + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let example1: int array = unbox<int array> [|0; 5; 3; 2; 2|]
        let example2: int array = unbox<int array> [|99|]
        let example3: int array = unbox<int array> [|-2; -5; -45|]
        printfn "%s" (_str (merge_insertion_sort (example1)))
        printfn "%s" (_str (merge_insertion_sort (example2)))
        printfn "%s" (_str (merge_insertion_sort (example3)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

// Generated 2025-08-08 11:10 +0700

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
type SortedLinkedList = {
    mutable _values: int array
}
let rec sort_list (nums: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable nums = nums
    try
        let mutable arr: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (nums)) do
            arr <- Array.append arr [|(_idx nums (i))|]
            i <- i + 1
        let mutable j: int = 0
        while j < (Seq.length (arr)) do
            let mutable k: int = j + 1
            while k < (Seq.length (arr)) do
                if (_idx arr (k)) < (_idx arr (j)) then
                    let tmp: int = _idx arr (j)
                    arr.[j] <- _idx arr (k)
                    arr.[k] <- tmp
                k <- k + 1
            j <- j + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and make_sorted_linked_list (ints: int array) =
    let mutable __ret : SortedLinkedList = Unchecked.defaultof<SortedLinkedList>
    let mutable ints = ints
    try
        __ret <- { _values = sort_list (ints) }
        raise Return
        __ret
    with
        | Return -> __ret
and len_sll (sll: SortedLinkedList) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable sll = sll
    try
        __ret <- Seq.length (sll._values)
        raise Return
        __ret
    with
        | Return -> __ret
and str_sll (sll: SortedLinkedList) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable sll = sll
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length (sll._values)) do
            res <- res + (_str (_idx (sll._values) (i)))
            if (i + 1) < (Seq.length (sll._values)) then
                res <- res + " -> "
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and merge_lists (a: SortedLinkedList) (b: SortedLinkedList) =
    let mutable __ret : SortedLinkedList = Unchecked.defaultof<SortedLinkedList>
    let mutable a = a
    let mutable b = b
    try
        let mutable combined: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (a._values)) do
            combined <- Array.append combined [|(_idx (a._values) (i))|]
            i <- i + 1
        i <- 0
        while i < (Seq.length (b._values)) do
            combined <- Array.append combined [|(_idx (b._values) (i))|]
            i <- i + 1
        __ret <- make_sorted_linked_list (combined)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let test_data_odd: int array = [|3; 9; -11; 0; 7; 5; 1; -1|]
        let test_data_even: int array = [|4; 6; 2; 0; 8; 10; 3; -2|]
        let sll_one: SortedLinkedList = make_sorted_linked_list (test_data_odd)
        let sll_two: SortedLinkedList = make_sorted_linked_list (test_data_even)
        let merged: SortedLinkedList = merge_lists (sll_one) (sll_two)
        printfn "%s" (_str (len_sll (merged)))
        printfn "%s" (str_sll (merged))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

// Generated 2025-08-09 16:21 +0700

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
type NumberContainer = {
    mutable _numbermap: System.Collections.Generic.IDictionary<int, int array>
    mutable _indexmap: System.Collections.Generic.IDictionary<int, int>
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec remove_at (xs: int array) (idx: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable idx = idx
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if i <> idx then
                res <- Array.append res [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec insert_at (xs: int array) (idx: int) (``val``: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable idx = idx
    let mutable ``val`` = ``val``
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if i = idx then
                res <- Array.append res [|``val``|]
            res <- Array.append res [|(_idx xs (int i))|]
            i <- i + 1
        if idx = (Seq.length (xs)) then
            res <- Array.append res [|``val``|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec binary_search_delete (array: int array) (item: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable array = array
    let mutable item = item
    try
        let mutable low: int = 0
        let mutable high: int = (Seq.length (array)) - 1
        let mutable arr: int array = array
        while low <= high do
            let mid: int = _floordiv (low + high) 2
            if (_idx arr (int mid)) = item then
                arr <- remove_at (arr) (mid)
                __ret <- arr
                raise Return
            else
                if (_idx arr (int mid)) < item then
                    low <- mid + 1
                else
                    high <- mid - 1
        printfn "%s" ("ValueError: Either the item is not in the array or the array was unsorted")
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec binary_search_insert (array: int array) (index: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable array = array
    let mutable index = index
    try
        let mutable low: int = 0
        let mutable high: int = (Seq.length (array)) - 1
        let mutable arr: int array = array
        while low <= high do
            let mid: int = _floordiv (low + high) 2
            if (_idx arr (int mid)) = index then
                arr <- insert_at (arr) (mid + 1) (index)
                __ret <- arr
                raise Return
            else
                if (_idx arr (int mid)) < index then
                    low <- mid + 1
                else
                    high <- mid - 1
        arr <- insert_at (arr) (low) (index)
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec change (cont: NumberContainer) (idx: int) (num: int) =
    let mutable __ret : NumberContainer = Unchecked.defaultof<NumberContainer>
    let mutable cont = cont
    let mutable idx = idx
    let mutable num = num
    try
        let mutable _numbermap: System.Collections.Generic.IDictionary<int, int array> = cont._numbermap
        let mutable _indexmap: System.Collections.Generic.IDictionary<int, int> = cont._indexmap
        if _indexmap.ContainsKey(idx) then
            let old: int = _dictGet _indexmap (idx)
            let indexes: int array = _dictGet _numbermap (old)
            if (Seq.length (indexes)) = 1 then
                _numbermap.[int old] <- Array.empty<int>
            else
                _numbermap.[int old] <- binary_search_delete (indexes) (idx)
        _indexmap.[int idx] <- num
        if _numbermap.ContainsKey(num) then
            _numbermap.[int num] <- binary_search_insert (_dictGet _numbermap (num)) (idx)
        else
            _numbermap.[int num] <- unbox<int array> [|idx|]
        __ret <- { _numbermap = _numbermap; _indexmap = _indexmap }
        raise Return
        __ret
    with
        | Return -> __ret
let rec find (cont: NumberContainer) (num: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable cont = cont
    let mutable num = num
    try
        let _numbermap: System.Collections.Generic.IDictionary<int, int array> = cont._numbermap
        if _numbermap.ContainsKey(num) then
            let mutable arr: int array = _dictGet _numbermap (num)
            if (Seq.length (arr)) > 0 then
                __ret <- _idx arr (int 0)
                raise Return
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let mutable nm: System.Collections.Generic.IDictionary<int, int array> = _dictCreate []
let mutable im: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
let mutable cont: NumberContainer = { _numbermap = nm; _indexmap = im }
printfn "%d" (find (cont) (10))
cont <- change (cont) (0) (10)
printfn "%d" (find (cont) (10))
cont <- change (cont) (0) (20)
printfn "%d" (find (cont) (10))
printfn "%d" (find (cont) (20))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

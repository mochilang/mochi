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
type LinkedList = {
    mutable _data: int array
}
let rec empty_list () =
    let mutable __ret : LinkedList = Unchecked.defaultof<LinkedList>
    try
        __ret <- { _data = [||] }
        raise Return
        __ret
    with
        | Return -> __ret
and push (list: LinkedList) (value: int) =
    let mutable __ret : LinkedList = Unchecked.defaultof<LinkedList>
    let mutable list = list
    let mutable value = value
    try
        let mutable res: int array = [|value|]
        res <- unbox<int array> (Array.append (res) (list._data))
        __ret <- { _data = res }
        raise Return
        __ret
    with
        | Return -> __ret
and swap_nodes (list: LinkedList) (v1: int) (v2: int) =
    let mutable __ret : LinkedList = Unchecked.defaultof<LinkedList>
    let mutable list = list
    let mutable v1 = v1
    let mutable v2 = v2
    try
        if v1 = v2 then
            __ret <- list
            raise Return
        let mutable idx1: int = 0 - 1
        let mutable idx2: int = 0 - 1
        let mutable i: int = 0
        while i < (Seq.length (list._data)) do
            if ((_idx (list._data) (i)) = v1) && (idx1 = (0 - 1)) then
                idx1 <- i
            if ((_idx (list._data) (i)) = v2) && (idx2 = (0 - 1)) then
                idx2 <- i
            i <- i + 1
        if (idx1 = (0 - 1)) || (idx2 = (0 - 1)) then
            __ret <- list
            raise Return
        let mutable res: int array = list._data
        let temp: int = _idx res (idx1)
        res.[idx1] <- _idx res (idx2)
        res.[idx2] <- temp
        __ret <- { _data = res }
        raise Return
        __ret
    with
        | Return -> __ret
and to_string (list: LinkedList) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable list = list
    try
        __ret <- _str (list._data)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable ll: LinkedList = empty_list()
        let mutable i: int = 5
        while i > 0 do
            ll <- push (ll) (i)
            i <- i - 1
        printfn "%s" ("Original Linked List: " + (to_string (ll)))
        ll <- swap_nodes (ll) (1) (4)
        printfn "%s" ("Modified Linked List: " + (to_string (ll)))
        printfn "%s" ("After swapping the nodes whose data is 1 and 4.")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

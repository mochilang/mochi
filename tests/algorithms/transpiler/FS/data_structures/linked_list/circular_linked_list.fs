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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type CircularLinkedList = {
    mutable _data: int array
}
type DeleteResult = {
    mutable _list: CircularLinkedList
    mutable _value: int
}
let rec empty_list () =
    let mutable __ret : CircularLinkedList = Unchecked.defaultof<CircularLinkedList>
    try
        __ret <- { _data = [||] }
        raise Return
        __ret
    with
        | Return -> __ret
and length (_list: CircularLinkedList) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable _list = _list
    try
        __ret <- Seq.length (_list._data)
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (_list: CircularLinkedList) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable _list = _list
    try
        __ret <- (Seq.length (_list._data)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and to_string (_list: CircularLinkedList) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable _list = _list
    try
        if (Seq.length (_list._data)) = 0 then
            __ret <- ""
            raise Return
        let mutable s: string = _str (_idx (_list._data) (0))
        let mutable i: int = 1
        while i < (Seq.length (_list._data)) do
            s <- (s + "->") + (_str (_idx (_list._data) (i)))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and insert_nth (_list: CircularLinkedList) (index: int) (_value: int) =
    let mutable __ret : CircularLinkedList = Unchecked.defaultof<CircularLinkedList>
    let mutable _list = _list
    let mutable index = index
    let mutable _value = _value
    try
        if (index < 0) || (index > (Seq.length (_list._data))) then
            failwith ("index out of range")
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < index do
            res <- Array.append res [|(_idx (_list._data) (i))|]
            i <- i + 1
        res <- Array.append res [|_value|]
        while i < (Seq.length (_list._data)) do
            res <- Array.append res [|(_idx (_list._data) (i))|]
            i <- i + 1
        __ret <- { _data = res }
        raise Return
        __ret
    with
        | Return -> __ret
and insert_head (_list: CircularLinkedList) (_value: int) =
    let mutable __ret : CircularLinkedList = Unchecked.defaultof<CircularLinkedList>
    let mutable _list = _list
    let mutable _value = _value
    try
        __ret <- insert_nth (_list) (0) (_value)
        raise Return
        __ret
    with
        | Return -> __ret
and insert_tail (_list: CircularLinkedList) (_value: int) =
    let mutable __ret : CircularLinkedList = Unchecked.defaultof<CircularLinkedList>
    let mutable _list = _list
    let mutable _value = _value
    try
        __ret <- insert_nth (_list) (Seq.length (_list._data)) (_value)
        raise Return
        __ret
    with
        | Return -> __ret
and delete_nth (_list: CircularLinkedList) (index: int) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable _list = _list
    let mutable index = index
    try
        if (index < 0) || (index >= (Seq.length (_list._data))) then
            failwith ("index out of range")
        let mutable res: int array = [||]
        let mutable i: int = 0
        let mutable ``val``: int = 0
        while i < (Seq.length (_list._data)) do
            if i = index then
                ``val`` <- _idx (_list._data) (i)
            else
                res <- Array.append res [|(_idx (_list._data) (i))|]
            i <- i + 1
        __ret <- { _list = { _data = res }; _value = ``val`` }
        raise Return
        __ret
    with
        | Return -> __ret
and delete_front (_list: CircularLinkedList) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable _list = _list
    try
        __ret <- delete_nth (_list) (0)
        raise Return
        __ret
    with
        | Return -> __ret
and delete_tail (_list: CircularLinkedList) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable _list = _list
    try
        __ret <- delete_nth (_list) ((Seq.length (_list._data)) - 1)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable cll: CircularLinkedList = empty_list()
        let mutable i: int = 0
        while i < 5 do
            cll <- insert_tail (cll) (i + 1)
            i <- i + 1
        printfn "%s" (to_string (cll))
        cll <- insert_tail (cll) (6)
        printfn "%s" (to_string (cll))
        cll <- insert_head (cll) (0)
        printfn "%s" (to_string (cll))
        let mutable res: DeleteResult = delete_front (cll)
        cll <- res._list
        printfn "%d" (res._value)
        res <- delete_tail (cll)
        cll <- res._list
        printfn "%d" (res._value)
        res <- delete_nth (cll) (2)
        cll <- res._list
        printfn "%d" (res._value)
        printfn "%s" (to_string (cll))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

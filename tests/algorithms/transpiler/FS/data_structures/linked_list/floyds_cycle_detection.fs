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
type LinkedList = {
    mutable _next: int array
    mutable _head: int
}
let NULL: int = 0 - 1
let rec empty_list () =
    let mutable __ret : LinkedList = Unchecked.defaultof<LinkedList>
    try
        __ret <- { _next = Array.empty<int>; _head = NULL }
        raise Return
        __ret
    with
        | Return -> __ret
and add_node (list: LinkedList) (value: int) =
    let mutable __ret : LinkedList = Unchecked.defaultof<LinkedList>
    let mutable list = list
    let mutable value = value
    try
        let mutable nexts: int array = list._next
        let new_index: int = Seq.length (nexts)
        nexts <- Array.append nexts [|NULL|]
        if (list._head) = NULL then
            __ret <- { _next = nexts; _head = new_index }
            raise Return
        let mutable last: int = list._head
        while (_idx nexts (last)) <> NULL do
            last <- _idx nexts (last)
        let mutable new_nexts: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (nexts)) do
            if i = last then
                new_nexts <- Array.append new_nexts [|new_index|]
            else
                new_nexts <- Array.append new_nexts [|(_idx nexts (i))|]
            i <- i + 1
        __ret <- { _next = new_nexts; _head = list._head }
        raise Return
        __ret
    with
        | Return -> __ret
and set_next (list: LinkedList) (index: int) (next_index: int) =
    let mutable __ret : LinkedList = Unchecked.defaultof<LinkedList>
    let mutable list = list
    let mutable index = index
    let mutable next_index = next_index
    try
        let mutable nexts: int array = list._next
        let mutable new_nexts: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (nexts)) do
            if i = index then
                new_nexts <- Array.append new_nexts [|next_index|]
            else
                new_nexts <- Array.append new_nexts [|(_idx nexts (i))|]
            i <- i + 1
        __ret <- { _next = new_nexts; _head = list._head }
        raise Return
        __ret
    with
        | Return -> __ret
and detect_cycle (list: LinkedList) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable list = list
    try
        if (list._head) = NULL then
            __ret <- false
            raise Return
        let mutable nexts: int array = list._next
        let mutable slow: int = list._head
        let mutable fast: int = list._head
        while (fast <> NULL) && ((_idx nexts (fast)) <> NULL) do
            slow <- _idx nexts (slow)
            fast <- _idx nexts (_idx nexts (fast))
            if slow = fast then
                __ret <- true
                raise Return
        __ret <- false
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
        ll <- add_node (ll) (1)
        ll <- add_node (ll) (2)
        ll <- add_node (ll) (3)
        ll <- add_node (ll) (4)
        ll <- set_next (ll) (3) (1)
        printfn "%b" (detect_cycle (ll))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

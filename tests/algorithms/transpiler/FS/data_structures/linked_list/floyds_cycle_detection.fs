// Generated 2025-08-07 14:57 +0700

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
type LinkedList = {
    next: int array
    head: int
}
let NULL: int = 0 - 1
let rec empty_list () =
    let mutable __ret : LinkedList = Unchecked.defaultof<LinkedList>
    try
        __ret <- { next = Array.empty<int>; head = NULL }
        raise Return
        __ret
    with
        | Return -> __ret
and add_node (list: LinkedList) (value: int) =
    let mutable __ret : LinkedList = Unchecked.defaultof<LinkedList>
    let mutable list = list
    let mutable value = value
    try
        let mutable nexts: int array = list.next
        let new_index: int = Seq.length (nexts)
        nexts <- Array.append nexts [|NULL|]
        if (list.head) = NULL then
            __ret <- { next = nexts; head = new_index }
            raise Return
        let mutable last: int = list.head
        while (_idx nexts (last)) <> NULL do
            last <- _idx nexts (last)
        let mutable new_nexts: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (nexts)) do
            if i = last then
                new_nexts <- Array.append new_nexts [|new_index|]
            else
                new_nexts <- Array.append new_nexts [|_idx nexts (i)|]
            i <- i + 1
        __ret <- { next = new_nexts; head = list.head }
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
        let mutable nexts: int array = list.next
        let mutable new_nexts: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (nexts)) do
            if i = index then
                new_nexts <- Array.append new_nexts [|next_index|]
            else
                new_nexts <- Array.append new_nexts [|_idx nexts (i)|]
            i <- i + 1
        __ret <- { next = new_nexts; head = list.head }
        raise Return
        __ret
    with
        | Return -> __ret
and detect_cycle (list: LinkedList) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable list = list
    try
        if (list.head) = NULL then
            __ret <- false
            raise Return
        let mutable nexts: int array = list.next
        let mutable slow: int = list.head
        let mutable fast: int = list.head
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

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
type Node = {
    mutable value: string
    mutable _next: int
}
type Stack = {
    mutable _nodes: Node array
    mutable _top: int
}
type PopResult = {
    mutable _stack: Stack
    mutable value: string
}
let rec empty_stack () =
    let mutable __ret : Stack = Unchecked.defaultof<Stack>
    try
        __ret <- { _nodes = [||]; _top = -1 }
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (_stack: Stack) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable _stack = _stack
    try
        __ret <- (_stack._top) = (-1)
        raise Return
        __ret
    with
        | Return -> __ret
and push (_stack: Stack) (item: string) =
    let mutable __ret : Stack = Unchecked.defaultof<Stack>
    let mutable _stack = _stack
    let mutable item = item
    try
        let new_node: Node = { value = item; _next = _stack._top }
        let mutable new_nodes: Node array = _stack._nodes
        new_nodes <- Array.append new_nodes [|new_node|]
        let new_top: int = (Seq.length (new_nodes)) - 1
        __ret <- { _nodes = new_nodes; _top = new_top }
        raise Return
        __ret
    with
        | Return -> __ret
and pop (_stack: Stack) =
    let mutable __ret : PopResult = Unchecked.defaultof<PopResult>
    let mutable _stack = _stack
    try
        if (_stack._top) = (-1) then
            failwith ("pop from empty stack")
        let node: Node = _idx (_stack._nodes) (_stack._top)
        let new_top: int = node._next
        let new_stack: Stack = { _nodes = _stack._nodes; _top = new_top }
        __ret <- { _stack = new_stack; value = node.value }
        raise Return
        __ret
    with
        | Return -> __ret
and peek (_stack: Stack) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable _stack = _stack
    try
        if (_stack._top) = (-1) then
            failwith ("peek from empty stack")
        let node: Node = _idx (_stack._nodes) (_stack._top)
        __ret <- node.value
        raise Return
        __ret
    with
        | Return -> __ret
and clear (_stack: Stack) =
    let mutable __ret : Stack = Unchecked.defaultof<Stack>
    let mutable _stack = _stack
    try
        __ret <- { _nodes = [||]; _top = -1 }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable _stack: Stack = empty_stack()
        printfn "%b" (is_empty (_stack))
        _stack <- push (_stack) ("5")
        _stack <- push (_stack) ("9")
        _stack <- push (_stack) ("python")
        printfn "%b" (is_empty (_stack))
        let mutable res: PopResult = pop (_stack)
        _stack <- res._stack
        printfn "%s" (res.value)
        _stack <- push (_stack) ("algorithms")
        res <- pop (_stack)
        _stack <- res._stack
        printfn "%s" (res.value)
        res <- pop (_stack)
        _stack <- res._stack
        printfn "%s" (res.value)
        res <- pop (_stack)
        _stack <- res._stack
        printfn "%s" (res.value)
        printfn "%b" (is_empty (_stack))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

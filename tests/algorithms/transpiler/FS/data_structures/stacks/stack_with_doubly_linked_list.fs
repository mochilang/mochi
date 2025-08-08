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
type Node = {
    mutable _data: int
    mutable _next: int
    mutable _prev: int
}
type Stack = {
    mutable _nodes: Node array
    mutable _head: int
}
type PopResult = {
    mutable _stack: Stack
    mutable value: int
    mutable ok: bool
}
type TopResult = {
    mutable value: int
    mutable ok: bool
}
let rec empty_stack () =
    let mutable __ret : Stack = Unchecked.defaultof<Stack>
    try
        __ret <- { _nodes = [||]; _head = 0 - 1 }
        raise Return
        __ret
    with
        | Return -> __ret
and push (_stack: Stack) (value: int) =
    let mutable __ret : Stack = Unchecked.defaultof<Stack>
    let mutable _stack = _stack
    let mutable value = value
    try
        let mutable _nodes: Node array = _stack._nodes
        let mutable idx: int = Seq.length (_nodes)
        let mutable new_node: Node = { _data = value; _next = _stack._head; _prev = 0 - 1 }
        _nodes <- Array.append _nodes [|new_node|]
        if (_stack._head) <> (0 - 1) then
            let mutable head_node: Node = _idx _nodes (_stack._head)
            head_node._prev <- idx
            _nodes.[_stack._head] <- head_node
        __ret <- { _nodes = _nodes; _head = idx }
        raise Return
        __ret
    with
        | Return -> __ret
and pop (_stack: Stack) =
    let mutable __ret : PopResult = Unchecked.defaultof<PopResult>
    let mutable _stack = _stack
    try
        if (_stack._head) = (0 - 1) then
            __ret <- { _stack = _stack; value = 0; ok = false }
            raise Return
        let mutable _nodes: Node array = _stack._nodes
        let mutable head_node: Node = _idx _nodes (_stack._head)
        let value: int = head_node._data
        let next_idx: int = head_node._next
        if next_idx <> (0 - 1) then
            let mutable next_node: Node = _idx _nodes (next_idx)
            next_node._prev <- 0 - 1
            _nodes.[next_idx] <- next_node
        let new_stack: Stack = { _nodes = _nodes; _head = next_idx }
        __ret <- { _stack = new_stack; value = value; ok = true }
        raise Return
        __ret
    with
        | Return -> __ret
and top (_stack: Stack) =
    let mutable __ret : TopResult = Unchecked.defaultof<TopResult>
    let mutable _stack = _stack
    try
        if (_stack._head) = (0 - 1) then
            __ret <- { value = 0; ok = false }
            raise Return
        let node: Node = _idx (_stack._nodes) (_stack._head)
        __ret <- { value = node._data; ok = true }
        raise Return
        __ret
    with
        | Return -> __ret
and size (_stack: Stack) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable _stack = _stack
    try
        let mutable count: int = 0
        let mutable idx: int = _stack._head
        while idx <> (0 - 1) do
            count <- count + 1
            let node: Node = _idx (_stack._nodes) (idx)
            idx <- node._next
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (_stack: Stack) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable _stack = _stack
    try
        __ret <- (_stack._head) = (0 - 1)
        raise Return
        __ret
    with
        | Return -> __ret
and print_stack (_stack: Stack) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable _stack = _stack
    try
        printfn "%s" ("stack elements are:")
        let mutable idx: int = _stack._head
        let mutable s: string = ""
        while idx <> (0 - 1) do
            let node: Node = _idx (_stack._nodes) (idx)
            s <- (s + (_str (node._data))) + "->"
            idx <- node._next
        if (String.length (s)) > 0 then
            printfn "%s" (s)
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable _stack: Stack = empty_stack()
        printfn "%s" ("Stack operations using Doubly LinkedList")
        _stack <- push (_stack) (4)
        _stack <- push (_stack) (5)
        _stack <- push (_stack) (6)
        _stack <- push (_stack) (7)
        print_stack (_stack)
        let t: TopResult = top (_stack)
        if t.ok then
            printfn "%s" ("Top element is " + (_str (t.value)))
        else
            printfn "%s" ("Top element is None")
        printfn "%s" ("Size of the stack is " + (_str (size (_stack))))
        let mutable p: PopResult = pop (_stack)
        _stack <- p._stack
        p <- pop (_stack)
        _stack <- p._stack
        print_stack (_stack)
        printfn "%s" ("stack is empty: " + (_str (is_empty (_stack))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

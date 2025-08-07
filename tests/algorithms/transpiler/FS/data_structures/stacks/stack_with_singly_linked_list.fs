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
type Node = {
    value: string
    next: int
}
type Stack = {
    nodes: Node array
    top: int
}
type PopResult = {
    stack: Stack
    value: string
}
let rec empty_stack () =
    let mutable __ret : Stack = Unchecked.defaultof<Stack>
    try
        __ret <- { nodes = [||]; top = -1 }
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (stack: Stack) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable stack = stack
    try
        __ret <- (stack.top) = (-1)
        raise Return
        __ret
    with
        | Return -> __ret
and push (stack: Stack) (item: string) =
    let mutable __ret : Stack = Unchecked.defaultof<Stack>
    let mutable stack = stack
    let mutable item = item
    try
        let new_node: Node = { value = item; next = stack.top }
        let mutable new_nodes: Node array = stack.nodes
        new_nodes <- Array.append new_nodes [|new_node|]
        let new_top: int = (Seq.length (new_nodes)) - 1
        __ret <- { nodes = new_nodes; top = new_top }
        raise Return
        __ret
    with
        | Return -> __ret
and pop (stack: Stack) =
    let mutable __ret : PopResult = Unchecked.defaultof<PopResult>
    let mutable stack = stack
    try
        if (stack.top) = (-1) then
            failwith ("pop from empty stack")
        let node: Node = _idx (stack.nodes) (stack.top)
        let new_top: int = node.next
        let new_stack: Stack = { nodes = stack.nodes; top = new_top }
        __ret <- { stack = new_stack; value = node.value }
        raise Return
        __ret
    with
        | Return -> __ret
and peek (stack: Stack) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable stack = stack
    try
        if (stack.top) = (-1) then
            failwith ("peek from empty stack")
        let node: Node = _idx (stack.nodes) (stack.top)
        __ret <- node.value
        raise Return
        __ret
    with
        | Return -> __ret
and clear (stack: Stack) =
    let mutable __ret : Stack = Unchecked.defaultof<Stack>
    let mutable stack = stack
    try
        __ret <- { nodes = [||]; top = -1 }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable stack: Stack = empty_stack()
        printfn "%b" (is_empty (stack))
        stack <- push (stack) ("5")
        stack <- push (stack) ("9")
        stack <- push (stack) ("python")
        printfn "%b" (is_empty (stack))
        let mutable res: PopResult = pop (stack)
        stack <- res.stack
        printfn "%s" (res.value)
        stack <- push (stack) ("algorithms")
        res <- pop (stack)
        stack <- res.stack
        printfn "%s" (res.value)
        res <- pop (stack)
        stack <- res.stack
        printfn "%s" (res.value)
        res <- pop (stack)
        stack <- res.stack
        printfn "%s" (res.value)
        printfn "%b" (is_empty (stack))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

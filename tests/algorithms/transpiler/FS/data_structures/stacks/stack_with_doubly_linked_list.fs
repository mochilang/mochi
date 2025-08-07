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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Node = {
    data: int
    next: int
    prev: int
}
type Stack = {
    nodes: Node array
    head: int
}
type PopResult = {
    stack: Stack
    value: int
    ok: bool
}
type TopResult = {
    value: int
    ok: bool
}
let rec empty_stack () =
    let mutable __ret : Stack = Unchecked.defaultof<Stack>
    try
        __ret <- { nodes = [||]; head = 0 - 1 }
        raise Return
        __ret
    with
        | Return -> __ret
and push (stack: Stack) (value: int) =
    let mutable __ret : Stack = Unchecked.defaultof<Stack>
    let mutable stack = stack
    let mutable value = value
    try
        let mutable nodes: Node array = stack.nodes
        let mutable idx: int = Seq.length (nodes)
        let mutable new_node: Node = { data = value; next = stack.head; prev = 0 - 1 }
        nodes <- Array.append nodes [|new_node|]
        if (stack.head) <> (0 - 1) then
            let mutable head_node: Node = _idx nodes (stack.head)
            head_node <- { head_node with prev = idx }
            nodes.[stack.head] <- head_node
        __ret <- { nodes = nodes; head = idx }
        raise Return
        __ret
    with
        | Return -> __ret
and pop (stack: Stack) =
    let mutable __ret : PopResult = Unchecked.defaultof<PopResult>
    let mutable stack = stack
    try
        if (stack.head) = (0 - 1) then
            __ret <- { stack = stack; value = 0; ok = false }
            raise Return
        let mutable nodes: Node array = stack.nodes
        let mutable head_node: Node = _idx nodes (stack.head)
        let value: int = head_node.data
        let next_idx: int = head_node.next
        if next_idx <> (0 - 1) then
            let mutable next_node: Node = _idx nodes (next_idx)
            next_node <- { next_node with prev = 0 - 1 }
            nodes.[next_idx] <- next_node
        let new_stack: Stack = { nodes = nodes; head = next_idx }
        __ret <- { stack = new_stack; value = value; ok = true }
        raise Return
        __ret
    with
        | Return -> __ret
and top (stack: Stack) =
    let mutable __ret : TopResult = Unchecked.defaultof<TopResult>
    let mutable stack = stack
    try
        if (stack.head) = (0 - 1) then
            __ret <- { value = 0; ok = false }
            raise Return
        let node: Node = _idx (stack.nodes) (stack.head)
        __ret <- { value = node.data; ok = true }
        raise Return
        __ret
    with
        | Return -> __ret
and size (stack: Stack) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable stack = stack
    try
        let mutable count: int = 0
        let mutable idx: int = stack.head
        while idx <> (0 - 1) do
            count <- count + 1
            let node: Node = _idx (stack.nodes) (idx)
            idx <- node.next
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (stack: Stack) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable stack = stack
    try
        __ret <- (stack.head) = (0 - 1)
        raise Return
        __ret
    with
        | Return -> __ret
and print_stack (stack: Stack) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable stack = stack
    try
        printfn "%s" ("stack elements are:")
        let mutable idx: int = stack.head
        let mutable s: string = ""
        while idx <> (0 - 1) do
            let node: Node = _idx (stack.nodes) (idx)
            s <- (s + (_str (node.data))) + "->"
            idx <- node.next
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
        let mutable stack: Stack = empty_stack()
        printfn "%s" ("Stack operations using Doubly LinkedList")
        stack <- push (stack) (4)
        stack <- push (stack) (5)
        stack <- push (stack) (6)
        stack <- push (stack) (7)
        print_stack (stack)
        let t: TopResult = top (stack)
        if t.ok then
            printfn "%s" ("Top element is " + (_str (t.value)))
        else
            printfn "%s" ("Top element is None")
        printfn "%s" ("Size of the stack is " + (_str (size (stack))))
        let mutable p: PopResult = pop (stack)
        stack <- p.stack
        p <- pop (stack)
        stack <- p.stack
        print_stack (stack)
        printfn "%s" ("stack is empty: " + (_str (is_empty (stack))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

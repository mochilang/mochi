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
}
let NIL: int = 0 - 1
let mutable nodes: Node array = [||]
let rec make_linked_list (elements: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable elements = elements
    try
        if (Seq.length (elements)) = 0 then
            failwith ("The Elements List is empty")
        nodes <- Array.empty<Node>
        nodes <- Array.append nodes [|{ data = _idx elements (0); next = NIL }|]
        let mutable head: int = 0
        let mutable current: int = head
        let mutable i: int = 1
        while i < (Seq.length (elements)) do
            nodes <- Array.append nodes [|{ data = _idx elements (i); next = NIL }|]
            (_idx nodes (current)).next <- (Seq.length (nodes)) - 1
            current <- (Seq.length (nodes)) - 1
            i <- i + 1
        __ret <- head
        raise Return
        __ret
    with
        | Return -> __ret
and node_to_string (head: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable head = head
    try
        let mutable s: string = ""
        let mutable index: int = head
        while index <> NIL do
            let node: Node = _idx nodes (index)
            s <- ((s + "<") + (_str (node.data))) + "> ---> "
            index <- node.next
        s <- s + "<END>"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let list_data: int array = [|1; 3; 5; 32; 44; 12; 43|]
        printfn "%s" ("List: " + (_str (list_data)))
        printfn "%s" ("Creating Linked List from List.")
        let head: int = make_linked_list (list_data)
        printfn "%s" ("Linked List:")
        printfn "%s" (node_to_string (head))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

// Generated 2025-08-07 15:46 +0700

exception Break
exception Continue

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
    key: int
    freq: int
}
let rec sort_nodes (nodes: Node array) =
    let mutable __ret : Node array = Unchecked.defaultof<Node array>
    let mutable nodes = nodes
    try
        let mutable arr: Node array = nodes
        let mutable i: int = 1
        try
            while i < (Seq.length (arr)) do
                try
                    let key_node: Node = _idx arr (i)
                    let mutable j: int = i - 1
                    try
                        while j >= 0 do
                            try
                                let temp: Node = _idx arr (j)
                                if (temp.key) > (key_node.key) then
                                    arr.[j + 1] <- temp
                                    j <- j - 1
                                else
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    arr.[j + 1] <- key_node
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and print_node (n: Node) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    try
        printfn "%s" (((("Node(key=" + (_str (n.key))) + ", freq=") + (_str (n.freq))) + ")")
        __ret
    with
        | Return -> __ret
and print_binary_search_tree (root: int array array) (keys: int array) (i: int) (j: int) (parent: int) (is_left: bool) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable root = root
    let mutable keys = keys
    let mutable i = i
    let mutable j = j
    let mutable parent = parent
    let mutable is_left = is_left
    try
        if ((i > j) || (i < 0)) || (j > ((Seq.length (root)) - 1)) then
            __ret <- ()
            raise Return
        let node: int = _idx (_idx root (i)) (j)
        if parent = (-1) then
            printfn "%s" ((_str (_idx keys (node))) + " is the root of the binary search tree.")
        else
            if is_left then
                printfn "%s" ((((_str (_idx keys (node))) + " is the left child of key ") + (_str (parent))) + ".")
            else
                printfn "%s" ((((_str (_idx keys (node))) + " is the right child of key ") + (_str (parent))) + ".")
        print_binary_search_tree (root) (keys) (i) (node - 1) (_idx keys (node)) (true)
        print_binary_search_tree (root) (keys) (node + 1) (j) (_idx keys (node)) (false)
        __ret
    with
        | Return -> __ret
and find_optimal_binary_search_tree (original_nodes: Node array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable original_nodes = original_nodes
    try
        let mutable nodes: Node array = sort_nodes (original_nodes)
        let n: int = Seq.length (nodes)
        let mutable keys: int array = [||]
        let mutable freqs: int array = [||]
        let mutable i: int = 0
        while i < n do
            let node: Node = _idx nodes (i)
            keys <- Array.append keys [|node.key|]
            freqs <- Array.append freqs [|node.freq|]
            i <- i + 1
        let mutable dp: int array array = [||]
        let mutable total: int array array = [||]
        let mutable root: int array array = [||]
        i <- 0
        while i < n do
            let mutable dp_row: int array = [||]
            let mutable total_row: int array = [||]
            let mutable root_row: int array = [||]
            let mutable j: int = 0
            while j < n do
                if i = j then
                    dp_row <- Array.append dp_row [|_idx freqs (i)|]
                    total_row <- Array.append total_row [|_idx freqs (i)|]
                    root_row <- Array.append root_row [|i|]
                else
                    dp_row <- Array.append dp_row [|0|]
                    total_row <- Array.append total_row [|0|]
                    root_row <- Array.append root_row [|0|]
                j <- j + 1
            dp <- Array.append dp [|dp_row|]
            total <- Array.append total [|total_row|]
            root <- Array.append root [|root_row|]
            i <- i + 1
        let mutable interval_length: int = 2
        let INF: int = 2147483647
        while interval_length <= n do
            i <- 0
            while i < ((n - interval_length) + 1) do
                let mutable j: int = (i + interval_length) - 1
                dp.[i].[j] <- INF
                total.[i].[j] <- (_idx (_idx total (i)) (j - 1)) + (_idx freqs (j))
                let mutable r: int = _idx (_idx root (i)) (j - 1)
                while r <= (_idx (_idx root (i + 1)) (j)) do
                    let left: int = if r <> i then (_idx (_idx dp (i)) (r - 1)) else 0
                    let right: int = if r <> j then (_idx (_idx dp (r + 1)) (j)) else 0
                    let cost: int = (left + (_idx (_idx total (i)) (j))) + right
                    if (_idx (_idx dp (i)) (j)) > cost then
                        dp.[i].[j] <- cost
                        root.[i].[j] <- r
                    r <- r + 1
                i <- i + 1
            interval_length <- interval_length + 1
        printfn "%s" ("Binary search tree nodes:")
        i <- 0
        while i < n do
            print_node (_idx nodes (i))
            i <- i + 1
        printfn "%s" (("\nThe cost of optimal BST for given tree nodes is " + (_str (_idx (_idx dp (0)) (n - 1)))) + ".")
        print_binary_search_tree (root) (keys) (0) (n - 1) (-1) (false)
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let nodes: Node array = [|{ key = 12; freq = 8 }; { key = 10; freq = 34 }; { key = 20; freq = 50 }; { key = 42; freq = 3 }; { key = 25; freq = 40 }; { key = 37; freq = 30 }|]
        find_optimal_binary_search_tree (nodes)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

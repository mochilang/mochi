// Generated 2025-08-13 07:12 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Node = {
    mutable _key: int
    mutable _freq: int
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
                    let key_node: Node = _idx arr (int i)
                    let mutable j: int = i - 1
                    try
                        while j >= 0 do
                            try
                                let temp: Node = _idx arr (int j)
                                if (temp._key) > (key_node._key) then
                                    arr.[(j + 1)] <- temp
                                    j <- j - 1
                                else
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    arr.[(j + 1)] <- key_node
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
        ignore (printfn "%s" (((("Node(key=" + (_str (n._key))) + ", freq=") + (_str (n._freq))) + ")"))
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
        let node: int = _idx (_idx root (int i)) (int j)
        if parent = (-1) then
            ignore (printfn "%s" ((_str (_idx keys (int node))) + " is the root of the binary search tree."))
        else
            if is_left then
                ignore (printfn "%s" ((((_str (_idx keys (int node))) + " is the left child of key ") + (_str (parent))) + "."))
            else
                ignore (printfn "%s" ((((_str (_idx keys (int node))) + " is the right child of key ") + (_str (parent))) + "."))
        print_binary_search_tree (root) (keys) (i) (node - 1) (_idx keys (int node)) (true)
        print_binary_search_tree (root) (keys) (node + 1) (j) (_idx keys (int node)) (false)
        __ret
    with
        | Return -> __ret
and find_optimal_binary_search_tree (original_nodes: Node array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable original_nodes = original_nodes
    try
        let mutable nodes: Node array = sort_nodes (original_nodes)
        let n: int = Seq.length (nodes)
        let mutable keys: int array = Array.empty<int>
        let mutable freqs: int array = Array.empty<int>
        let mutable i: int = 0
        while i < n do
            let node: Node = _idx nodes (int i)
            keys <- Array.append keys [|(node._key)|]
            freqs <- Array.append freqs [|(node._freq)|]
            i <- i + 1
        let mutable dp: int array array = Array.empty<int array>
        let mutable total: int array array = Array.empty<int array>
        let mutable root: int array array = Array.empty<int array>
        i <- 0
        while i < n do
            let mutable dp_row: int array = Array.empty<int>
            let mutable total_row: int array = Array.empty<int>
            let mutable root_row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < n do
                if i = j then
                    dp_row <- Array.append dp_row [|(_idx freqs (int i))|]
                    total_row <- Array.append total_row [|(_idx freqs (int i))|]
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
                total.[i].[j] <- (_idx (_idx total (int i)) (int (j - 1))) + (_idx freqs (int j))
                let mutable r: int = _idx (_idx root (int i)) (int (j - 1))
                while r <= (_idx (_idx root (int (i + 1))) (int j)) do
                    let left: int = if r <> i then (_idx (_idx dp (int i)) (int (r - 1))) else 0
                    let right: int = if r <> j then (_idx (_idx dp (int (r + 1))) (int j)) else 0
                    let cost: int = (left + (_idx (_idx total (int i)) (int j))) + right
                    if (_idx (_idx dp (int i)) (int j)) > cost then
                        dp.[i].[j] <- cost
                        root.[i].[j] <- r
                    r <- r + 1
                i <- i + 1
            interval_length <- interval_length + 1
        ignore (printfn "%s" ("Binary search tree nodes:"))
        i <- 0
        while i < n do
            print_node (_idx nodes (int i))
            i <- i + 1
        ignore (printfn "%s" (("\nThe cost of optimal BST for given tree nodes is " + (_str (_idx (_idx dp (int 0)) (int (n - 1))))) + "."))
        print_binary_search_tree (root) (keys) (0) (n - 1) (-1) (false)
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let nodes: Node array = unbox<Node array> [|{ _key = 12; _freq = 8 }; { _key = 10; _freq = 34 }; { _key = 20; _freq = 50 }; { _key = 42; _freq = 3 }; { _key = 25; _freq = 40 }; { _key = 37; _freq = 30 }|]
        find_optimal_binary_search_tree (nodes)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

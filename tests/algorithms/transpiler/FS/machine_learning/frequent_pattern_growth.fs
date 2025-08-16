// Generated 2025-08-16 11:48 +0700

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
open System.Collections.Generic

let rec make_node (name: string) (count: int) (parent: obj) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable name = name
    let mutable count = count
    let mutable parent = parent
    try
        __ret <- (_dictCreate [("name", box (name)); ("count", box (count)); ("parent", box (parent)); ("children", box (_dictCreate [])); ("node_link", box (null))])
        raise Return
        __ret
    with
        | Return -> __ret
and update_header (node_to_test: obj) (target_node: obj) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable node_to_test = node_to_test
    let mutable target_node = target_node
    try
        let mutable current: obj = node_to_test
        while (((current :?> System.Collections.Generic.IDictionary<string, obj>).["node_link"])) <> null do
            current <- ((current :?> System.Collections.Generic.IDictionary<string, obj>).["node_link"])
        current <- _dictAdd (unbox<System.Collections.Generic.IDictionary<string, obj>> current) (string ("node_link")) (target_node)
        __ret
    with
        | Return -> __ret
and update_tree (items: string array) (in_tree: obj) (header_table: obj) (count: int) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable items = items
    let mutable in_tree = in_tree
    let mutable header_table = header_table
    let mutable count = count
    try
        let first: string = _idx items (int 0)
        let mutable children: obj = box (((in_tree :?> System.Collections.Generic.IDictionary<string, obj>).["children"]))
        if Seq.contains first children then
            let mutable child: obj = box (((children :?> System.Collections.Generic.IDictionary<string, obj>).[first]))
            child <- _dictAdd (unbox<System.Collections.Generic.IDictionary<string, obj>> child) (string ("count")) (box (((child :?> System.Collections.Generic.IDictionary<string, obj>).[("count" + (_str (count)))])))
            children <- _dictAdd (unbox<System.Collections.Generic.IDictionary<string, obj>> children) (string (first)) (child)
            in_tree <- _dictAdd (unbox<System.Collections.Generic.IDictionary<string, obj>> in_tree) (string ("children")) (children)
        else
            let mutable new_node: obj = make_node (first) (count) (in_tree)
            children <- _dictAdd (unbox<System.Collections.Generic.IDictionary<string, obj>> children) (string (first)) (new_node)
            in_tree <- _dictAdd (unbox<System.Collections.Generic.IDictionary<string, obj>> in_tree) (string ("children")) (children)
            let mutable entry: obj = box (((header_table :?> System.Collections.Generic.IDictionary<string, obj>).[first]))
            if (((entry :?> System.Collections.Generic.IDictionary<string, obj>).["node"])) = null then
                entry <- _dictAdd (unbox<System.Collections.Generic.IDictionary<string, obj>> entry) (string ("node")) (new_node)
            else
                ignore (update_header ((((entry :?> System.Collections.Generic.IDictionary<string, obj>).["node"]))) (new_node))
            header_table <- _dictAdd (unbox<System.Collections.Generic.IDictionary<string, obj>> header_table) (string (first)) (entry)
        if (Seq.length (items)) > 1 then
            let rest: string array = Array.sub items 1 ((Seq.length (items)) - 1)
            ignore (update_tree (rest) (((children :?> System.Collections.Generic.IDictionary<string, obj>).[first])) (header_table) (count))
        __ret
    with
        | Return -> __ret
and sort_items (items: string array) (header_table: obj) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable items = items
    let mutable header_table = header_table
    try
        let mutable arr: string array = items
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            let mutable j: int = i + 1
            while j < (Seq.length (arr)) do
                if (_idx (((header_table :?> System.Collections.Generic.IDictionary<string, obj>).[(_idx arr (int i))])) (int "count")) < (_idx (((header_table :?> System.Collections.Generic.IDictionary<string, obj>).[(_idx arr (int j))])) (int "count")) then
                    let tmp: string = _idx arr (int i)
                    arr.[i] <- _idx arr (int j)
                    arr.[j] <- tmp
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and create_tree (data_set: string array array) (min_sup: int) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable data_set = data_set
    let mutable min_sup = min_sup
    try
        let mutable counts = _dictCreate []
        let mutable i: int = 0
        while i < (Seq.length (data_set)) do
            let trans: string array = _idx data_set (int i)
            let mutable j: int = 0
            while j < (Seq.length (trans)) do
                let item: string = _idx trans (int j)
                if counts.ContainsKey(item) then
                    _dictAdd (unbox<System.Collections.Generic.IDictionary<string, obj>> counts) (string (item)) (_dictGet counts (item + (_str (1))))
                else
                    _dictAdd (unbox<System.Collections.Generic.IDictionary<string, obj>> counts) (string (item)) (1)
                j <- j + 1
            i <- i + 1
        let mutable header_table = _dictCreate []
        for KeyValue(k, _) in counts do
            let cnt: obj = _dictGet counts (k)
            if (unbox<int> cnt) >= min_sup then
                _dictAdd (unbox<System.Collections.Generic.IDictionary<string, obj>> header_table) (string (k)) ((_dictCreate [("count", box (cnt)); ("node", box (null))]))
        let mutable freq_items = [||]
        for KeyValue(k, _) in header_table do
            freq_items <- Array.append freq_items [|k|]
        if (Seq.length (freq_items)) = 0 then
            __ret <- (_dictCreate [("tree", unbox<map> (make_node ("Null Set") (1) (null))); ("header", _dictCreate [])])
            raise Return
        let mutable fp_tree: obj = make_node ("Null Set") (1) (null)
        i <- 0
        while i < (Seq.length (data_set)) do
            let tran: string array = _idx data_set (int i)
            let mutable local_items = [||]
            let mutable j: int = 0
            while j < (Seq.length (tran)) do
                let item: string = _idx tran (int j)
                if header_table.ContainsKey(item) then
                    local_items <- Array.append local_items [|item|]
                j <- j + 1
            if (Seq.length (local_items)) > 0 then
                local_items <- sort_items (local_items) (header_table)
                ignore (update_tree (local_items) (fp_tree) (header_table) (1))
            i <- i + 1
        __ret <- (_dictCreate [("tree", unbox<map> fp_tree); ("header", header_table)])
        raise Return
        __ret
    with
        | Return -> __ret
and ascend_tree (leaf_node: obj) (path: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable leaf_node = leaf_node
    let mutable path = path
    try
        let mutable prefix: string array = path
        if (((leaf_node :?> System.Collections.Generic.IDictionary<string, obj>).["parent"])) <> null then
            prefix <- Array.append prefix [|unbox<string> (((leaf_node :?> System.Collections.Generic.IDictionary<string, obj>).["name"]))|]
            prefix <- ascend_tree (((leaf_node :?> System.Collections.Generic.IDictionary<string, obj>).["parent"])) (prefix)
        else
            prefix <- Array.append prefix [|unbox<string> (((leaf_node :?> System.Collections.Generic.IDictionary<string, obj>).["name"]))|]
        __ret <- prefix
        raise Return
        __ret
    with
        | Return -> __ret
and find_prefix_path (base_pat: string) (tree_node: obj) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable base_pat = base_pat
    let mutable tree_node = tree_node
    try
        let mutable cond_pats = [||]
        let mutable node: obj = tree_node
        while node <> null do
            let mutable prefix: string array = ascend_tree (node) (Array.empty<string>)
            if (Seq.length (prefix)) > 1 then
                let mutable items: string array = Array.sub prefix 1 ((Seq.length (prefix)) - 1)
                cond_pats <- Array.append cond_pats [|(_dictCreate [("items", box (items)); ("count", box (((node :?> System.Collections.Generic.IDictionary<string, obj>).["count"])))])|]
            node <- ((node :?> System.Collections.Generic.IDictionary<string, obj>).["node_link"])
        __ret <- cond_pats
        raise Return
        __ret
    with
        | Return -> __ret
and mine_tree (in_tree: obj) (header_table: obj) (min_sup: int) (pre_fix: string array) (freq_item_list: string array array) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable in_tree = in_tree
    let mutable header_table = header_table
    let mutable min_sup = min_sup
    let mutable pre_fix = pre_fix
    let mutable freq_item_list = freq_item_list
    try
        let mutable freq_list: string array array = freq_item_list
        let mutable items = [||]
        for k in header_table do
            items <- Array.append items [|(unbox<array> k)|]
        let mutable sorted_items: array array = items
        let mutable i: int = 0
        while i < (Seq.length (sorted_items)) do
            let mutable j: int = i + 1
            while j < (Seq.length (sorted_items)) do
                if (_idx (((header_table :?> System.Collections.Generic.IDictionary<string, obj>).[(_idx sorted_items (int i))])) (int "count")) > (_idx (((header_table :?> System.Collections.Generic.IDictionary<string, obj>).[(_idx sorted_items (int j))])) (int "count")) then
                    let tmp = _idx sorted_items (int i)
                    sorted_items.[i] <- _idx sorted_items (int j)
                    sorted_items.[j] <- tmp
                j <- j + 1
            i <- i + 1
        let mutable idx: int = 0
        while idx < (Seq.length (sorted_items)) do
            let base_pat = _idx sorted_items (int idx)
            let mutable new_freq: string array = pre_fix
            new_freq <- Array.append new_freq [|base_pat|]
            freq_list <- Array.append freq_list [|new_freq|]
            let mutable cond_pats: obj array = find_prefix_path (unbox<string> base_pat) ((_idx (((header_table :?> System.Collections.Generic.IDictionary<string, obj>).[base_pat])) (int "node")))
            let mutable cond_dataset = [||]
            let mutable p: int = 0
            while p < (Seq.length (cond_pats)) do
                let pat: obj = _idx cond_pats (int p)
                let mutable r: int = 0
                while r < (int (((pat :?> System.Collections.Generic.IDictionary<string, obj>).["count"]))) do
                    cond_dataset <- Array.append cond_dataset [|(((pat :?> System.Collections.Generic.IDictionary<string, obj>).["items"]))|]
                    r <- r + 1
                p <- p + 1
            let res2: obj = create_tree (unbox<string array array> cond_dataset) (min_sup)
            let my_tree: obj = box (((res2 :?> System.Collections.Generic.IDictionary<string, obj>).["tree"]))
            let my_head: obj = box (((res2 :?> System.Collections.Generic.IDictionary<string, obj>).["header"]))
            if (int ((unbox<System.Array> my_head).Length)) > 0 then
                freq_list <- mine_tree (my_tree) (my_head) (min_sup) (new_freq) (freq_list)
            idx <- idx + 1
        __ret <- freq_list
        raise Return
        __ret
    with
        | Return -> __ret
and list_to_string (xs: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            s <- s + (_idx xs (int i))
            if i < ((Seq.length (xs)) - 1) then
                s <- s + ", "
            i <- i + 1
        __ret <- s + "]"
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let data_set: string array array = [|[|"bread"; "milk"; "cheese"|]; [|"bread"; "milk"|]; [|"bread"; "diapers"|]; [|"bread"; "milk"; "diapers"|]; [|"milk"; "diapers"|]; [|"milk"; "cheese"|]; [|"diapers"; "cheese"|]; [|"bread"; "milk"; "cheese"; "diapers"|]|]
        let res: obj = create_tree (data_set) (3)
        let fp_tree: obj = box (((res :?> System.Collections.Generic.IDictionary<string, obj>).["tree"]))
        let header_table: obj = box (((res :?> System.Collections.Generic.IDictionary<string, obj>).["header"]))
        let mutable freq_items = [||]
        freq_items <- mine_tree (fp_tree) (header_table) (3) (Array.empty<string>) (unbox<string array array> freq_items)
        ignore (printfn "%d" (Seq.length (data_set)))
        ignore (printfn "%A" ((unbox<System.Array> header_table).Length))
        let mutable i: int = 0
        while i < (Seq.length (freq_items)) do
            ignore (printfn "%s" (list_to_string (_idx freq_items (int i))))
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())

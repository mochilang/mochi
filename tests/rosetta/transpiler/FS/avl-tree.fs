// Generated 2025-07-26 04:38 +0700

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
let rec Node (data: int) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable data = data
    try
        __ret <- Map.ofList [("Data", box data); ("Balance", box 0); ("Link", box [|null; null|])]
        raise Return
        __ret
    with
        | Return -> __ret
and getLink (n: Map<string, obj>) (dir: int) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable n = n
    let mutable dir = dir
    try
        __ret <- (unbox<obj array> (n.["Link"])).[dir]
        raise Return
        __ret
    with
        | Return -> __ret
and setLink (n: Map<string, obj>) (dir: int) (v: obj) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    let mutable dir = dir
    let mutable v = v
    try
        let mutable links: obj array = unbox<obj array> (n.["Link"])
        links.[dir] <- v
        n <- Map.add "Link" links n
        __ret
    with
        | Return -> __ret
and opp (dir: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable dir = dir
    try
        __ret <- 1 - dir
        raise Return
        __ret
    with
        | Return -> __ret
and single (root: Map<string, obj>) (dir: int) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable root = root
    let mutable dir = dir
    try
        let mutable tmp = getLink root (int (opp dir))
        setLink root (int (opp dir)) (getLink (unbox<Map<string, obj>> tmp) dir)
        setLink (unbox<Map<string, obj>> tmp) dir root
        __ret <- tmp
        raise Return
        __ret
    with
        | Return -> __ret
and double (root: Map<string, obj>) (dir: int) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable root = root
    let mutable dir = dir
    try
        let mutable tmp = getLink (unbox<Map<string, obj>> (getLink root (int (opp dir)))) dir
        setLink (unbox<Map<string, obj>> (getLink root (int (opp dir)))) dir (getLink (unbox<Map<string, obj>> tmp) (int (opp dir)))
        setLink (unbox<Map<string, obj>> tmp) (int (opp dir)) (getLink root (int (opp dir)))
        setLink root (int (opp dir)) tmp
        tmp <- getLink root (int (opp dir))
        setLink root (int (opp dir)) (getLink (unbox<Map<string, obj>> tmp) dir)
        setLink (unbox<Map<string, obj>> tmp) dir root
        __ret <- tmp
        raise Return
        __ret
    with
        | Return -> __ret
and adjustBalance (root: Map<string, obj>) (dir: int) (bal: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable root = root
    let mutable dir = dir
    let mutable bal = bal
    try
        let mutable n: Map<string, obj> = unbox<Map<string, obj>> (getLink root dir)
        let mutable nn: Map<string, obj> = unbox<Map<string, obj>> (getLink n (int (opp dir)))
        if (int (nn.["Balance"])) = 0 then
            root <- Map.add "Balance" 0 root
            n <- Map.add "Balance" 0 n
        else
            if (int (nn.["Balance"])) = bal then
                root <- Map.add "Balance" (-bal) root
                n <- Map.add "Balance" 0 n
            else
                root <- Map.add "Balance" 0 root
                n <- Map.add "Balance" bal n
        nn <- Map.add "Balance" 0 nn
        __ret
    with
        | Return -> __ret
and insertBalance (root: Map<string, obj>) (dir: int) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable root = root
    let mutable dir = dir
    try
        let mutable n: Map<string, obj> = unbox<Map<string, obj>> (getLink root dir)
        let mutable bal: int = (2 * dir) - 1
        if (int (n.["Balance"])) = bal then
            root <- Map.add "Balance" 0 root
            n <- Map.add "Balance" 0 n
            __ret <- single root (int (opp dir))
            raise Return
        adjustBalance root dir bal
        __ret <- double root (int (opp dir))
        raise Return
        __ret
    with
        | Return -> __ret
and insertR (root: obj) (data: int) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable root = root
    let mutable data = data
    try
        if root = null then
            __ret <- Map.ofList [("node", box (Node data)); ("done", box false)]
            raise Return
        let mutable node: Map<string, obj> = unbox<Map<string, obj>> root
        let mutable dir: int = 0
        if (int (node.["Data"])) < data then
            dir <- 1
        let mutable r: Map<string, obj> = insertR (getLink node dir) data
        setLink node dir (r.["node"])
        if unbox<bool> (r.["done"]) then
            __ret <- Map.ofList [("node", box node); ("done", box true)]
            raise Return
        node <- Map.add "Balance" ((int (node.["Balance"])) + ((2 * dir) - 1)) node
        if (int (node.["Balance"])) = 0 then
            __ret <- Map.ofList [("node", box node); ("done", box true)]
            raise Return
        if ((int (node.["Balance"])) = 1) || ((int (node.["Balance"])) = (-1)) then
            __ret <- Map.ofList [("node", box node); ("done", box false)]
            raise Return
        __ret <- Map.ofList [("node", box (insertBalance node dir)); ("done", box true)]
        raise Return
        __ret
    with
        | Return -> __ret
and Insert (tree: obj) (data: int) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable tree = tree
    let mutable data = data
    try
        let r: Map<string, obj> = insertR tree data
        __ret <- r.["node"]
        raise Return
        __ret
    with
        | Return -> __ret
and removeBalance (root: Map<string, obj>) (dir: int) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable root = root
    let mutable dir = dir
    try
        let mutable n: Map<string, obj> = unbox<Map<string, obj>> (getLink root (int (opp dir)))
        let mutable bal: int = (2 * dir) - 1
        if (int (n.["Balance"])) = (-bal) then
            root <- Map.add "Balance" 0 root
            n <- Map.add "Balance" 0 n
            __ret <- Map.ofList [("node", box (single root dir)); ("done", box false)]
            raise Return
        if (int (n.["Balance"])) = bal then
            adjustBalance root (int (opp dir)) (-bal)
            __ret <- Map.ofList [("node", box (double root dir)); ("done", box false)]
            raise Return
        root <- Map.add "Balance" (-bal) root
        n <- Map.add "Balance" bal n
        __ret <- Map.ofList [("node", box (single root dir)); ("done", box true)]
        raise Return
        __ret
    with
        | Return -> __ret
and removeR (root: obj) (data: int) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable root = root
    let mutable data = data
    try
        if root = null then
            __ret <- Map.ofList [("node", box null); ("done", box false)]
            raise Return
        let mutable node: Map<string, obj> = unbox<Map<string, obj>> root
        if (int (node.["Data"])) = data then
            if (getLink node 0) = null then
                __ret <- Map.ofList [("node", box (getLink node 1)); ("done", box false)]
                raise Return
            if (getLink node 1) = null then
                __ret <- Map.ofList [("node", box (getLink node 0)); ("done", box false)]
                raise Return
            let mutable heir = getLink node 0
            while (getLink (unbox<Map<string, obj>> heir) 1) <> null do
                heir <- getLink (unbox<Map<string, obj>> heir) 1
            node <- Map.add "Data" (heir.["Data"]) node
            data <- int (heir.["Data"])
        let mutable dir: int = 0
        if (int (node.["Data"])) < data then
            dir <- 1
        let mutable r: Map<string, obj> = removeR (getLink node dir) data
        setLink node dir (r.["node"])
        if unbox<bool> (r.["done"]) then
            __ret <- Map.ofList [("node", box node); ("done", box true)]
            raise Return
        node <- Map.add "Balance" (((int (node.["Balance"])) + 1) - (2 * dir)) node
        if ((int (node.["Balance"])) = 1) || ((int (node.["Balance"])) = (-1)) then
            __ret <- Map.ofList [("node", box node); ("done", box true)]
            raise Return
        if (int (node.["Balance"])) = 0 then
            __ret <- Map.ofList [("node", box node); ("done", box false)]
            raise Return
        __ret <- removeBalance node dir
        raise Return
        __ret
    with
        | Return -> __ret
and Remove (tree: obj) (data: int) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable tree = tree
    let mutable data = data
    try
        let r: Map<string, obj> = removeR tree data
        __ret <- r.["node"]
        raise Return
        __ret
    with
        | Return -> __ret
and indentStr (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < n do
            s <- s + " "
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and dumpNode (node: obj) (indent: int) (comma: bool) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable node = node
    let mutable indent = indent
    let mutable comma = comma
    try
        let sp: string = indentStr indent
        if node = null then
            let mutable line: string = sp + "null"
            if comma then
                line <- line + ","
            printfn "%s" line
        else
            printfn "%s" (sp + "{")
            printfn "%s" ((((unbox<string> (indentStr (indent + 3))) + "\"Data\": ") + (string (((node :?> Map<string, obj>).["Data"])))) + ",")
            printfn "%s" ((((unbox<string> (indentStr (indent + 3))) + "\"Balance\": ") + (string (((node :?> Map<string, obj>).["Balance"])))) + ",")
            printfn "%s" ((unbox<string> (indentStr (indent + 3))) + "\"Link\": [")
            dumpNode (getLink (unbox<Map<string, obj>> node) 0) (indent + 6) true
            dumpNode (getLink (unbox<Map<string, obj>> node) 1) (indent + 6) false
            printfn "%s" ((unbox<string> (indentStr (indent + 3))) + "]")
            let mutable ``end``: string = sp + "}"
            if comma then
                ``end`` <- ``end`` + ","
            printfn "%s" ``end``
        __ret
    with
        | Return -> __ret
and dump (node: obj) (indent: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable node = node
    let mutable indent = indent
    try
        dumpNode node indent false
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable tree = null
        printfn "%s" "Empty tree:"
        dump tree 0
        printfn "%s" ""
        printfn "%s" "Insert test:"
        tree <- Insert tree 3
        tree <- Insert tree 1
        tree <- Insert tree 4
        tree <- Insert tree 1
        tree <- Insert tree 5
        dump tree 0
        printfn "%s" ""
        printfn "%s" "Remove test:"
        tree <- Remove tree 3
        tree <- Remove tree 1
        let mutable t: Map<string, obj> = unbox<Map<string, obj>> tree
        t <- Map.add "Balance" 0 t
        tree <- t
        dump tree 0
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

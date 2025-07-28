// Generated 2025-07-28 10:03 +0700

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
let rec newList () =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    try
        __ret <- unbox<Map<string, obj>> (Map.ofList [("nodes", box (Map.ofList [])); ("head", box 0); ("tail", box 0); ("nextID", box 1)])
        raise Return
        __ret
    with
        | Return -> __ret
and newNode (l: Map<string, obj>) (v: obj) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable l = l
    let mutable v = v
    try
        let id: int = int (l.["nextID"])
        l <- Map.add "nextID" (box (id + 1)) l
        let mutable nodes: Map<int, Map<string, obj>> = unbox<Map<int, Map<string, obj>>> (l.["nodes"])
        let n = Map.ofList [("id", box id); ("value", box v); ("next", box 0); ("prev", box 0)]
        nodes <- Map.add id n nodes
        l <- Map.add "nodes" (box nodes) l
        __ret <- unbox<Map<string, obj>> n
        raise Return
        __ret
    with
        | Return -> __ret
and pushFront (l: Map<string, obj>) (v: obj) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable l = l
    let mutable v = v
    try
        let mutable n: Map<string, obj> = newNode l v
        n <- Map.add "next" (box (l.["head"])) n
        if (int (l.["head"])) <> 0 then
            let mutable nodes: Map<int, Map<string, obj>> = unbox<Map<int, Map<string, obj>>> (l.["nodes"])
            let mutable h: obj> = nodes.[(int (l.["head"]))] |> unbox<obj>>
            h.["prev"] <- n.["id"]
            nodes <- Map.add (int (h.["id"])) h nodes
            l <- Map.add "nodes" (box nodes) l
        else
            l <- Map.add "tail" (box (n.["id"])) l
        l <- Map.add "head" (box (n.["id"])) l
        let mutable nodes2: Map<int, Map<string, obj>> = unbox<Map<int, Map<string, obj>>> (l.["nodes"])
        nodes2 <- Map.add (int (n.["id"])) n nodes2
        l <- Map.add "nodes" (box nodes2) l
        __ret <- unbox<Map<string, obj>> n
        raise Return
        __ret
    with
        | Return -> __ret
and pushBack (l: Map<string, obj>) (v: obj) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable l = l
    let mutable v = v
    try
        let mutable n: Map<string, obj> = newNode l v
        n <- Map.add "prev" (box (l.["tail"])) n
        if (int (l.["tail"])) <> 0 then
            let mutable nodes: Map<int, Map<string, obj>> = unbox<Map<int, Map<string, obj>>> (l.["nodes"])
            let mutable t: obj> = nodes.[(int (l.["tail"]))] |> unbox<obj>>
            t.["next"] <- n.["id"]
            nodes <- Map.add (int (t.["id"])) t nodes
            l <- Map.add "nodes" (box nodes) l
        else
            l <- Map.add "head" (box (n.["id"])) l
        l <- Map.add "tail" (box (n.["id"])) l
        let mutable nodes2: Map<int, Map<string, obj>> = unbox<Map<int, Map<string, obj>>> (l.["nodes"])
        nodes2 <- Map.add (int (n.["id"])) n nodes2
        l <- Map.add "nodes" (box nodes2) l
        __ret <- unbox<Map<string, obj>> n
        raise Return
        __ret
    with
        | Return -> __ret
and insertBefore (l: Map<string, obj>) (refID: int) (v: obj) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable l = l
    let mutable refID = refID
    let mutable v = v
    try
        if refID = 0 then
            __ret <- pushFront l v
            raise Return
        let mutable nodes: Map<int, Map<string, obj>> = unbox<Map<int, Map<string, obj>>> (l.["nodes"])
        let mutable ref: obj> = nodes.[refID] |> unbox<obj>>
        let mutable n: Map<string, obj> = newNode l v
        n <- Map.add "prev" (box (ref.["prev"])) n
        n <- Map.add "next" (box (ref.["id"])) n
        if (int (ref.["prev"])) <> 0 then
            let mutable p: obj> = nodes.[(int (ref.["prev"]))] |> unbox<obj>>
            p.["next"] <- n.["id"]
            nodes <- Map.add (int (p.["id"])) p nodes
        else
            l <- Map.add "head" (box (n.["id"])) l
        ref.["prev"] <- n.["id"]
        nodes <- Map.add refID ref nodes
        nodes <- Map.add (int (n.["id"])) n nodes
        l <- Map.add "nodes" (box nodes) l
        __ret <- unbox<Map<string, obj>> n
        raise Return
        __ret
    with
        | Return -> __ret
and insertAfter (l: Map<string, obj>) (refID: int) (v: obj) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable l = l
    let mutable refID = refID
    let mutable v = v
    try
        if refID = 0 then
            __ret <- pushBack l v
            raise Return
        let mutable nodes: Map<int, Map<string, obj>> = unbox<Map<int, Map<string, obj>>> (l.["nodes"])
        let mutable ref: obj> = nodes.[refID] |> unbox<obj>>
        let mutable n: Map<string, obj> = newNode l v
        n <- Map.add "next" (box (ref.["next"])) n
        n <- Map.add "prev" (box (ref.["id"])) n
        if (int (ref.["next"])) <> 0 then
            let mutable nx: obj> = nodes.[(int (ref.["next"]))] |> unbox<obj>>
            nx.["prev"] <- n.["id"]
            nodes <- Map.add (int (nx.["id"])) nx nodes
        else
            l <- Map.add "tail" (box (n.["id"])) l
        ref.["next"] <- n.["id"]
        nodes <- Map.add refID ref nodes
        nodes <- Map.add (int (n.["id"])) n nodes
        l <- Map.add "nodes" (box nodes) l
        __ret <- unbox<Map<string, obj>> n
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable l: Map<string, obj> = newList()
        let mutable e4: Map<string, obj> = pushBack l 4
        let mutable e1: Map<string, obj> = pushFront l 1
        insertBefore l (int (e4.["id"])) 3
        insertAfter l (int (e1.["id"])) "two"
        let mutable id: int = int (l.["head"])
        let mutable nodes: Map<int, Map<string, obj>> = unbox<Map<int, Map<string, obj>>> (l.["nodes"])
        while id <> 0 do
            let node: obj> = nodes.[id] |> unbox<obj>>
            printfn "%s" (string (node.["value"]))
            id <- int (node.["next"])
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

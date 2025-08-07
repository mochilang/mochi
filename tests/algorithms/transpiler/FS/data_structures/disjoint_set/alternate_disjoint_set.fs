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
type DisjointSet = {
    set_counts: int array
    max_set: int
    ranks: int array
    parents: int array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec max_list (xs: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    try
        let mutable m: int = _idx xs (0)
        let mutable i: int = 1
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) > m then
                m <- _idx xs (i)
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
let rec disjoint_set_new (set_counts: int array) =
    let mutable __ret : DisjointSet = Unchecked.defaultof<DisjointSet>
    let mutable set_counts = set_counts
    try
        let max_set: int = max_list (set_counts)
        let num_sets: int = Seq.length (set_counts)
        let mutable ranks: int array = [||]
        let mutable parents: int array = [||]
        let mutable i: int = 0
        while i < num_sets do
            ranks <- Array.append ranks [|1|]
            parents <- Array.append parents [|i|]
            i <- i + 1
        __ret <- { set_counts = set_counts; max_set = max_set; ranks = ranks; parents = parents }
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_parent (ds: DisjointSet) (idx: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ds = ds
    let mutable idx = idx
    try
        if (_idx (ds.parents) (idx)) = idx then
            __ret <- idx
            raise Return
        let mutable parents: int array = ds.parents
        parents.[idx] <- get_parent (ds) (_idx parents (idx))
        ds <- { ds with parents = parents }
        __ret <- _idx (ds.parents) (idx)
        raise Return
        __ret
    with
        | Return -> __ret
let rec merge (ds: DisjointSet) (src: int) (dst: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ds = ds
    let mutable src = src
    let mutable dst = dst
    try
        let src_parent: int = get_parent (ds) (src)
        let dst_parent: int = get_parent (ds) (dst)
        if src_parent = dst_parent then
            __ret <- false
            raise Return
        if (_idx (ds.ranks) (dst_parent)) >= (_idx (ds.ranks) (src_parent)) then
            let mutable counts: int array = ds.set_counts
            counts.[dst_parent] <- (_idx counts (dst_parent)) + (_idx counts (src_parent))
            counts.[src_parent] <- 0
            ds <- { ds with set_counts = counts }
            let mutable parents: int array = ds.parents
            parents.[src_parent] <- dst_parent
            ds <- { ds with parents = parents }
            if (_idx (ds.ranks) (dst_parent)) = (_idx (ds.ranks) (src_parent)) then
                let mutable ranks: int array = ds.ranks
                ranks.[dst_parent] <- (_idx ranks (dst_parent)) + 1
                ds <- { ds with ranks = ranks }
            let joined: int = _idx (ds.set_counts) (dst_parent)
            if joined > (ds.max_set) then
                ds <- { ds with max_set = joined }
        else
            let mutable counts: int array = ds.set_counts
            counts.[src_parent] <- (_idx counts (src_parent)) + (_idx counts (dst_parent))
            counts.[dst_parent] <- 0
            ds <- { ds with set_counts = counts }
            let mutable parents: int array = ds.parents
            parents.[dst_parent] <- src_parent
            ds <- { ds with parents = parents }
            let joined: int = _idx (ds.set_counts) (src_parent)
            if joined > (ds.max_set) then
                ds <- { ds with max_set = joined }
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let mutable ds: DisjointSet = disjoint_set_new (unbox<int array> [|1; 1; 1|])
printfn "%b" (merge (ds) (1) (2))
printfn "%b" (merge (ds) (0) (2))
printfn "%b" (merge (ds) (0) (1))
printfn "%d" (get_parent (ds) (0))
printfn "%d" (get_parent (ds) (1))
printfn "%d" (ds.max_set)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

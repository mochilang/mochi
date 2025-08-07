// Generated 2025-08-07 10:31 +0700

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
type Tree = {
    values: int array
    lefts: int array
    rights: int array
    root: int
}
type Pair = {
    idx: int
    hd: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let NIL: int = 0 - 1
let rec make_tree () =
    let mutable __ret : Tree = Unchecked.defaultof<Tree>
    try
        __ret <- { values = [|3; 9; 20; 15; 7|]; lefts = [|1; NIL; 3; NIL; NIL|]; rights = [|2; NIL; 4; NIL; NIL|]; root = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
let rec index_of (xs: int array) (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) = x then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- NIL
        raise Return
        __ret
    with
        | Return -> __ret
let rec sort_pairs (hds: int array) (vals: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable hds = hds
    let mutable vals = vals
    try
        let mutable i: int = 0
        while i < (Seq.length (hds)) do
            let mutable j: int = i
            while (j > 0) && ((_idx hds (j - 1)) > (_idx hds (j))) do
                let hd_tmp: int = _idx hds (j - 1)
                hds.[j - 1] <- _idx hds (j)
                hds.[j] <- hd_tmp
                let val_tmp: int = _idx vals (j - 1)
                vals.[j - 1] <- _idx vals (j)
                vals.[j] <- val_tmp
                j <- j - 1
            i <- i + 1
        __ret
    with
        | Return -> __ret
let rec right_view (t: Tree) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable t = t
    try
        let mutable res: int array = [||]
        let mutable queue: int array = [|t.root|]
        while (Seq.length (queue)) > 0 do
            let size: int = Seq.length (queue)
            let mutable i: int = 0
            while i < size do
                let idx: int = _idx queue (i)
                if (_idx (t.lefts) (idx)) <> NIL then
                    queue <- Array.append queue [|_idx (t.lefts) (idx)|]
                if (_idx (t.rights) (idx)) <> NIL then
                    queue <- Array.append queue [|_idx (t.rights) (idx)|]
                i <- i + 1
            res <- Array.append res [|_idx (t.values) (_idx queue (size - 1))|]
            queue <- Array.sub queue size ((Seq.length (queue)) - size)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec left_view (t: Tree) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable t = t
    try
        let mutable res: int array = [||]
        let mutable queue: int array = [|t.root|]
        while (Seq.length (queue)) > 0 do
            let size: int = Seq.length (queue)
            let mutable i: int = 0
            while i < size do
                let idx: int = _idx queue (i)
                if (_idx (t.lefts) (idx)) <> NIL then
                    queue <- Array.append queue [|_idx (t.lefts) (idx)|]
                if (_idx (t.rights) (idx)) <> NIL then
                    queue <- Array.append queue [|_idx (t.rights) (idx)|]
                i <- i + 1
            res <- Array.append res [|_idx (t.values) (_idx queue (0))|]
            queue <- Array.sub queue size ((Seq.length (queue)) - size)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec top_view (t: Tree) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable t = t
    try
        let mutable hds: int array = [||]
        let mutable vals: int array = [||]
        let mutable queue_idx: int array = [|t.root|]
        let mutable queue_hd: int array = [|0|]
        while (Seq.length (queue_idx)) > 0 do
            let idx: int = _idx queue_idx (0)
            queue_idx <- Array.sub queue_idx 1 ((Seq.length (queue_idx)) - 1)
            let hd: int = _idx queue_hd (0)
            queue_hd <- Array.sub queue_hd 1 ((Seq.length (queue_hd)) - 1)
            if (index_of (hds) (hd)) = NIL then
                hds <- Array.append hds [|hd|]
                vals <- Array.append vals [|_idx (t.values) (idx)|]
            if (_idx (t.lefts) (idx)) <> NIL then
                queue_idx <- Array.append queue_idx [|_idx (t.lefts) (idx)|]
                queue_hd <- Array.append queue_hd [|hd - 1|]
            if (_idx (t.rights) (idx)) <> NIL then
                queue_idx <- Array.append queue_idx [|_idx (t.rights) (idx)|]
                queue_hd <- Array.append queue_hd [|hd + 1|]
        sort_pairs (hds) (vals)
        __ret <- vals
        raise Return
        __ret
    with
        | Return -> __ret
let rec bottom_view (t: Tree) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable t = t
    try
        let mutable hds: int array = [||]
        let mutable vals: int array = [||]
        let mutable queue_idx: int array = [|t.root|]
        let mutable queue_hd: int array = [|0|]
        while (Seq.length (queue_idx)) > 0 do
            let idx: int = _idx queue_idx (0)
            queue_idx <- Array.sub queue_idx 1 ((Seq.length (queue_idx)) - 1)
            let hd: int = _idx queue_hd (0)
            queue_hd <- Array.sub queue_hd 1 ((Seq.length (queue_hd)) - 1)
            let pos: int = index_of (hds) (hd)
            if pos = NIL then
                hds <- Array.append hds [|hd|]
                vals <- Array.append vals [|_idx (t.values) (idx)|]
            else
                vals.[pos] <- _idx (t.values) (idx)
            if (_idx (t.lefts) (idx)) <> NIL then
                queue_idx <- Array.append queue_idx [|_idx (t.lefts) (idx)|]
                queue_hd <- Array.append queue_hd [|hd - 1|]
            if (_idx (t.rights) (idx)) <> NIL then
                queue_idx <- Array.append queue_idx [|_idx (t.rights) (idx)|]
                queue_hd <- Array.append queue_hd [|hd + 1|]
        sort_pairs (hds) (vals)
        __ret <- vals
        raise Return
        __ret
    with
        | Return -> __ret
let tree: Tree = make_tree()
printfn "%s" (_repr (right_view (tree)))
printfn "%s" (_repr (left_view (tree)))
printfn "%s" (_repr (top_view (tree)))
printfn "%s" (_repr (bottom_view (tree)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

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
type RBTree = {
    nodes: int array array
    root: int
}
let LABEL: int = 0
let COLOR: int = 1
let PARENT: int = 2
let LEFT: int = 3
let RIGHT: int = 4
let NEG_ONE: int = -1
let rec make_tree () =
    let mutable __ret : RBTree = Unchecked.defaultof<RBTree>
    try
        __ret <- { nodes = [||]; root = -1 }
        raise Return
        __ret
    with
        | Return -> __ret
and rotate_left (t: RBTree) (x: int) =
    let mutable __ret : RBTree = Unchecked.defaultof<RBTree>
    let mutable t = t
    let mutable x = x
    try
        let mutable nodes: int array array = t.nodes
        let mutable y: int = _idx (_idx nodes (x)) (RIGHT)
        let yLeft: int = _idx (_idx nodes (y)) (LEFT)
        nodes.[x].[RIGHT] <- yLeft
        if yLeft <> NEG_ONE then
            nodes.[yLeft].[PARENT] <- x
        let xParent: int = _idx (_idx nodes (x)) (PARENT)
        nodes.[y].[PARENT] <- xParent
        if xParent = NEG_ONE then
            t <- { t with root = y }
        else
            if x = (_idx (_idx nodes (xParent)) (LEFT)) then
                nodes.[xParent].[LEFT] <- y
            else
                nodes.[xParent].[RIGHT] <- y
        nodes.[y].[LEFT] <- x
        nodes.[x].[PARENT] <- y
        t <- { t with nodes = nodes }
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
and rotate_right (t: RBTree) (x: int) =
    let mutable __ret : RBTree = Unchecked.defaultof<RBTree>
    let mutable t = t
    let mutable x = x
    try
        let mutable nodes: int array array = t.nodes
        let mutable y: int = _idx (_idx nodes (x)) (LEFT)
        let yRight: int = _idx (_idx nodes (y)) (RIGHT)
        nodes.[x].[LEFT] <- yRight
        if yRight <> NEG_ONE then
            nodes.[yRight].[PARENT] <- x
        let xParent: int = _idx (_idx nodes (x)) (PARENT)
        nodes.[y].[PARENT] <- xParent
        if xParent = NEG_ONE then
            t <- { t with root = y }
        else
            if x = (_idx (_idx nodes (xParent)) (RIGHT)) then
                nodes.[xParent].[RIGHT] <- y
            else
                nodes.[xParent].[LEFT] <- y
        nodes.[y].[RIGHT] <- x
        nodes.[x].[PARENT] <- y
        t <- { t with nodes = nodes }
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
and insert_fix (t: RBTree) (z: int) =
    let mutable __ret : RBTree = Unchecked.defaultof<RBTree>
    let mutable t = t
    let mutable z = z
    try
        let mutable nodes: int array array = t.nodes
        while (z <> (t.root)) && ((_idx (_idx nodes (_idx (_idx nodes (z)) (PARENT))) (COLOR)) = 1) do
            if (_idx (_idx nodes (z)) (PARENT)) = (_idx (_idx nodes (_idx (_idx nodes (_idx (_idx nodes (z)) (PARENT))) (PARENT))) (LEFT)) then
                let mutable y: int = _idx (_idx nodes (_idx (_idx nodes (_idx (_idx nodes (z)) (PARENT))) (PARENT))) (RIGHT)
                if (y <> NEG_ONE) && ((_idx (_idx nodes (y)) (COLOR)) = 1) then
                    nodes.[_idx (_idx nodes (z)) (PARENT)].[COLOR] <- 0
                    nodes.[y].[COLOR] <- 0
                    let gp: int = _idx (_idx nodes (_idx (_idx nodes (z)) (PARENT))) (PARENT)
                    nodes.[gp].[COLOR] <- 1
                    z <- gp
                else
                    if z = (_idx (_idx nodes (_idx (_idx nodes (z)) (PARENT))) (RIGHT)) then
                        z <- _idx (_idx nodes (z)) (PARENT)
                        t <- { t with nodes = nodes }
                        t <- rotate_left (t) (z)
                        nodes <- t.nodes
                    nodes.[_idx (_idx nodes (z)) (PARENT)].[COLOR] <- 0
                    let gp: int = _idx (_idx nodes (_idx (_idx nodes (z)) (PARENT))) (PARENT)
                    nodes.[gp].[COLOR] <- 1
                    t <- { t with nodes = nodes }
                    t <- rotate_right (t) (gp)
                    nodes <- t.nodes
            else
                let mutable y: int = _idx (_idx nodes (_idx (_idx nodes (_idx (_idx nodes (z)) (PARENT))) (PARENT))) (LEFT)
                if (y <> NEG_ONE) && ((_idx (_idx nodes (y)) (COLOR)) = 1) then
                    nodes.[_idx (_idx nodes (z)) (PARENT)].[COLOR] <- 0
                    nodes.[y].[COLOR] <- 0
                    let gp: int = _idx (_idx nodes (_idx (_idx nodes (z)) (PARENT))) (PARENT)
                    nodes.[gp].[COLOR] <- 1
                    z <- gp
                else
                    if z = (_idx (_idx nodes (_idx (_idx nodes (z)) (PARENT))) (LEFT)) then
                        z <- _idx (_idx nodes (z)) (PARENT)
                        t <- { t with nodes = nodes }
                        t <- rotate_right (t) (z)
                        nodes <- t.nodes
                    nodes.[_idx (_idx nodes (z)) (PARENT)].[COLOR] <- 0
                    let gp: int = _idx (_idx nodes (_idx (_idx nodes (z)) (PARENT))) (PARENT)
                    nodes.[gp].[COLOR] <- 1
                    t <- { t with nodes = nodes }
                    t <- rotate_left (t) (gp)
                    nodes <- t.nodes
        nodes <- t.nodes
        nodes.[t.root].[COLOR] <- 0
        t <- { t with nodes = nodes }
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
and tree_insert (t: RBTree) (v: int) =
    let mutable __ret : RBTree = Unchecked.defaultof<RBTree>
    let mutable t = t
    let mutable v = v
    try
        let mutable nodes: int array array = t.nodes
        let node: int array = [|v; 1; -1; -1; -1|]
        nodes <- Array.append nodes [|node|]
        let idx: int = (Seq.length (nodes)) - 1
        let mutable y: int = NEG_ONE
        let mutable x: int = t.root
        while x <> NEG_ONE do
            y <- x
            if v < (_idx (_idx nodes (x)) (LABEL)) then
                x <- _idx (_idx nodes (x)) (LEFT)
            else
                x <- _idx (_idx nodes (x)) (RIGHT)
        nodes.[idx].[PARENT] <- y
        if y = NEG_ONE then
            t <- { t with root = idx }
        else
            if v < (_idx (_idx nodes (y)) (LABEL)) then
                nodes.[y].[LEFT] <- idx
            else
                nodes.[y].[RIGHT] <- idx
        t <- { t with nodes = nodes }
        t <- insert_fix (t) (idx)
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
and inorder (t: RBTree) (x: int) (acc: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable t = t
    let mutable x = x
    let mutable acc = acc
    try
        if x = NEG_ONE then
            __ret <- acc
            raise Return
        acc <- inorder (t) (_idx (_idx (t.nodes) (x)) (LEFT)) (acc)
        acc <- Array.append acc [|_idx (_idx (t.nodes) (x)) (LABEL)|]
        acc <- inorder (t) (_idx (_idx (t.nodes) (x)) (RIGHT)) (acc)
        __ret <- acc
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable t: RBTree = make_tree()
        let values: int array = [|10; 20; 30; 15; 25; 5; 1|]
        let mutable i: int = 0
        while i < (Seq.length (values)) do
            t <- tree_insert (t) (_idx values (i))
            i <- i + 1
        let mutable res: int array = [||]
        res <- inorder (t) (t.root) (res)
        printfn "%s" (_str (res))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

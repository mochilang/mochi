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
type DS = {
    parent: int array
    rank: int array
}
type FindResult = {
    ds: DS
    root: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_set (ds: DS) (x: int) =
    let mutable __ret : DS = Unchecked.defaultof<DS>
    let mutable ds = ds
    let mutable x = x
    try
        let mutable p: int array = ds.parent
        let mutable r: int array = ds.rank
        p.[x] <- x
        r.[x] <- 0
        __ret <- { parent = p; rank = r }
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_set (ds: DS) (x: int) =
    let mutable __ret : FindResult = Unchecked.defaultof<FindResult>
    let mutable ds = ds
    let mutable x = x
    try
        if (_idx (ds.parent) (x)) = x then
            __ret <- { ds = ds; root = x }
            raise Return
        let res: FindResult = find_set (ds) (_idx (ds.parent) (x))
        let mutable p: int array = (res.ds).parent
        p.[x] <- res.root
        __ret <- { ds = { parent = p; rank = (res.ds).rank }; root = res.root }
        raise Return
        __ret
    with
        | Return -> __ret
let rec union_set (ds: DS) (x: int) (y: int) =
    let mutable __ret : DS = Unchecked.defaultof<DS>
    let mutable ds = ds
    let mutable x = x
    let mutable y = y
    try
        let fx: FindResult = find_set (ds) (x)
        let ds1: DS = fx.ds
        let x_root: int = fx.root
        let fy: FindResult = find_set (ds1) (y)
        let ds2: DS = fy.ds
        let y_root: int = fy.root
        if x_root = y_root then
            __ret <- ds2
            raise Return
        let mutable p: int array = ds2.parent
        let mutable r: int array = ds2.rank
        if (_idx r (x_root)) > (_idx r (y_root)) then
            p.[y_root] <- x_root
        else
            p.[x_root] <- y_root
            if (_idx r (x_root)) = (_idx r (y_root)) then
                r.[y_root] <- (_idx r (y_root)) + 1
        __ret <- { parent = p; rank = r }
        raise Return
        __ret
    with
        | Return -> __ret
let rec same_python_set (a: int) (b: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        if (a < 3) && (b < 3) then
            __ret <- true
            raise Return
        if (((a >= 3) && (a < 6)) && (b >= 3)) && (b < 6) then
            __ret <- true
            raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let mutable ds: DS = { parent = [||]; rank = [||] }
let mutable i: int = 0
while i < 6 do
    ds <- { ds with parent = Array.append (ds.parent) [|0|] }
    ds <- { ds with rank = Array.append (ds.rank) [|0|] }
    ds <- make_set (ds) (i)
    i <- i + 1
ds <- union_set (ds) (0) (1)
ds <- union_set (ds) (1) (2)
ds <- union_set (ds) (3) (4)
ds <- union_set (ds) (3) (5)
i <- 0
while i < 6 do
    let mutable j: int = 0
    while j < 6 do
        let res_i: FindResult = find_set (ds) (i)
        ds <- res_i.ds
        let root_i: int = res_i.root
        let res_j: FindResult = find_set (ds) (j)
        ds <- res_j.ds
        let root_j: int = res_j.root
        let same: bool = same_python_set (i) (j)
        let root_same: bool = root_i = root_j
        if same then
            if not root_same then
                failwith ("nodes should be in same set")
        else
            if root_same then
                failwith ("nodes should be in different sets")
        j <- j + 1
    i <- i + 1
i <- 0
while i < 6 do
    let res: FindResult = find_set (ds) (i)
    ds <- res.ds
    printfn "%s" (_str (res.root))
    i <- i + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

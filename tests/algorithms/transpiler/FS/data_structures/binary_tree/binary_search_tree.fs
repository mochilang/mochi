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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec create_node (value: int) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable value = value
    try
        __ret <- [|box (value); box (null); box (null)|]
        raise Return
        __ret
    with
        | Return -> __ret
and insert (node: obj array) (value: int) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable node = node
    let mutable value = value
    try
        if node = null then
            __ret <- create_node (value)
            raise Return
        if value < (unbox<int> (_idx node (0))) then
            node.[1] <- insert (_idx node (1)) (value)
        else
            if value > (unbox<int> (_idx node (0))) then
                node.[2] <- insert (_idx node (2)) (value)
        __ret <- node
        raise Return
        __ret
    with
        | Return -> __ret
and search (node: obj array) (value: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable node = node
    let mutable value = value
    try
        if node = null then
            __ret <- false
            raise Return
        if value = (unbox<int> (_idx node (0))) then
            __ret <- true
            raise Return
        if value < (unbox<int> (_idx node (0))) then
            __ret <- search (_idx node (1)) (value)
            raise Return
        __ret <- search (_idx node (2)) (value)
        raise Return
        __ret
    with
        | Return -> __ret
and inorder (node: obj array) (acc: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable node = node
    let mutable acc = acc
    try
        if node = null then
            __ret <- acc
            raise Return
        let left_acc: int array = inorder (_idx node (1)) (acc)
        let with_node: int array = Array.append left_acc [|unbox<int> (_idx node (0))|]
        __ret <- inorder (_idx node (2)) (with_node)
        raise Return
        __ret
    with
        | Return -> __ret
and find_min (node: obj array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable node = node
    try
        let mutable current: obj = box (box (node))
        while (_idx current (1)) <> null do
            current <- box (_idx current (1))
        __ret <- unbox<int> (((current :?> System.Array).GetValue(0)))
        raise Return
        __ret
    with
        | Return -> __ret
and find_max (node: obj array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable node = node
    try
        let mutable current: obj = box (node)
        while (_idx current (2)) <> null do
            current <- box (_idx current (2))
        __ret <- unbox<int> (((current :?> System.Array).GetValue(0)))
        raise Return
        __ret
    with
        | Return -> __ret
and delete (node: obj array) (value: int) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable node = node
    let mutable value = value
    try
        if node = null then
            __ret <- unbox<obj array> null
            raise Return
        if value < (unbox<int> (_idx node (0))) then
            node.[1] <- delete (_idx node (1)) (value)
        else
            if value > (unbox<int> (_idx node (0))) then
                node.[2] <- delete (_idx node (2)) (value)
            else
                if (_idx node (1)) = null then
                    __ret <- unbox<obj array> (_idx node (2))
                    raise Return
                if (_idx node (2)) = null then
                    __ret <- unbox<obj array> (_idx node (1))
                    raise Return
                let min_val: int = find_min (unbox<obj array> (_idx node (2)))
                node.[0] <- min_val
                node.[2] <- delete (_idx node (2)) (min_val)
        __ret <- node
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable root: obj array = Unchecked.defaultof<obj array>
        let nums: int array = [|8; 3; 6; 1; 10; 14; 13; 4; 7|]
        for v in nums do
            root <- insert (root) (v)
        printfn "%s" (_str (inorder (root) (Array.empty<int>)))
        printfn "%b" (search (root) (6))
        printfn "%b" (search (root) (20))
        printfn "%d" (find_min (root))
        printfn "%d" (find_max (root))
        root <- delete (root) (6)
        printfn "%s" (_str (inorder (root) (Array.empty<int>)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

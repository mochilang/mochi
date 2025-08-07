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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
open System.Collections.Generic

let NIL: int = 0 - 1
let mutable nodes: System.Collections.Generic.IDictionary<string, int> array = [||]
let rec new_node (value: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable value = value
    try
        let node: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("data", value); ("left", NIL); ("right", NIL); ("height", 1)]
        nodes <- Array.append nodes [|node|]
        __ret <- (Seq.length (nodes)) - 1
        raise Return
        __ret
    with
        | Return -> __ret
and get_height (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    try
        __ret <- if i = NIL then 0 else (_idx nodes (i).[(string ("height"))])
        raise Return
        __ret
    with
        | Return -> __ret
and my_max (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a > b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
and update_height (i: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable i = i
    try
        nodes.[i].["height"] <- (my_max (get_height (_idx nodes (i).[(string ("left"))])) (get_height (_idx nodes (i).[(string ("right"))]))) + 1
        __ret
    with
        | Return -> __ret
and right_rotation (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    try
        let left: int = _idx nodes (i).[(string ("left"))]
        nodes.[i].["left"] <- _idx nodes (left).[(string ("right"))]
        nodes.[left].["right"] <- i
        update_height (i)
        update_height (left)
        __ret <- left
        raise Return
        __ret
    with
        | Return -> __ret
and left_rotation (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    try
        let right: int = _idx nodes (i).[(string ("right"))]
        nodes.[i].["right"] <- _idx nodes (right).[(string ("left"))]
        nodes.[right].["left"] <- i
        update_height (i)
        update_height (right)
        __ret <- right
        raise Return
        __ret
    with
        | Return -> __ret
and lr_rotation (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    try
        nodes.[i].["left"] <- left_rotation (_idx nodes (i).[(string ("left"))])
        __ret <- right_rotation (i)
        raise Return
        __ret
    with
        | Return -> __ret
and rl_rotation (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    try
        nodes.[i].["right"] <- right_rotation (_idx nodes (i).[(string ("right"))])
        __ret <- left_rotation (i)
        raise Return
        __ret
    with
        | Return -> __ret
and insert_node (i: int) (value: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    let mutable value = value
    try
        if i = NIL then
            __ret <- new_node (value)
            raise Return
        if value < (_idx nodes (i).[(string ("data"))]) then
            nodes.[i].["left"] <- insert_node (_idx nodes (i).[(string ("left"))]) (value)
            if ((get_height (_idx nodes (i).[(string ("left"))])) - (get_height (_idx nodes (i).[(string ("right"))]))) = 2 then
                if value < (_idx nodes (_idx nodes (i).[(string ("left"))]).[(string ("data"))]) then
                    i <- right_rotation (i)
                else
                    i <- lr_rotation (i)
        else
            nodes.[i].["right"] <- insert_node (_idx nodes (i).[(string ("right"))]) (value)
            if ((get_height (_idx nodes (i).[(string ("right"))])) - (get_height (_idx nodes (i).[(string ("left"))]))) = 2 then
                if value < (_idx nodes (_idx nodes (i).[(string ("right"))]).[(string ("data"))]) then
                    i <- rl_rotation (i)
                else
                    i <- left_rotation (i)
        update_height (i)
        __ret <- i
        raise Return
        __ret
    with
        | Return -> __ret
and get_left_most (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    try
        let mutable cur: int = i
        while (_idx nodes (cur).[(string ("left"))]) <> NIL do
            cur <- _idx nodes (cur).[(string ("left"))]
        __ret <- _idx nodes (cur).[(string ("data"))]
        raise Return
        __ret
    with
        | Return -> __ret
and del_node (i: int) (value: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    let mutable value = value
    try
        if i = NIL then
            __ret <- NIL
            raise Return
        if value < (_idx nodes (i).[(string ("data"))]) then
            nodes.[i].["left"] <- del_node (_idx nodes (i).[(string ("left"))]) (value)
        else
            if value > (_idx nodes (i).[(string ("data"))]) then
                nodes.[i].["right"] <- del_node (_idx nodes (i).[(string ("right"))]) (value)
            else
                if ((_idx nodes (i).[(string ("left"))]) <> NIL) && ((_idx nodes (i).[(string ("right"))]) <> NIL) then
                    let temp: int = get_left_most (_idx nodes (i).[(string ("right"))])
                    nodes.[i].["data"] <- temp
                    nodes.[i].["right"] <- del_node (_idx nodes (i).[(string ("right"))]) (temp)
                else
                    if (_idx nodes (i).[(string ("left"))]) <> NIL then
                        i <- _idx nodes (i).[(string ("left"))]
                    else
                        i <- _idx nodes (i).[(string ("right"))]
        if i = NIL then
            __ret <- NIL
            raise Return
        let lh: int = get_height (_idx nodes (i).[(string ("left"))])
        let rh: int = get_height (_idx nodes (i).[(string ("right"))])
        if (rh - lh) = 2 then
            if (get_height (_idx nodes (_idx nodes (i).[(string ("right"))]).[(string ("right"))])) > (get_height (_idx nodes (_idx nodes (i).[(string ("right"))]).[(string ("left"))])) then
                i <- left_rotation (i)
            else
                i <- rl_rotation (i)
        else
            if (lh - rh) = 2 then
                if (get_height (_idx nodes (_idx nodes (i).[(string ("left"))]).[(string ("left"))])) > (get_height (_idx nodes (_idx nodes (i).[(string ("left"))]).[(string ("right"))])) then
                    i <- right_rotation (i)
                else
                    i <- lr_rotation (i)
        update_height (i)
        __ret <- i
        raise Return
        __ret
    with
        | Return -> __ret
and inorder (i: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable i = i
    try
        if i = NIL then
            __ret <- ""
            raise Return
        let left: string = inorder (_idx nodes (i).[(string ("left"))])
        let right: string = inorder (_idx nodes (i).[(string ("right"))])
        let mutable res: string = _str (_idx nodes (i).[(string ("data"))])
        if left <> "" then
            res <- (left + " ") + res
        if right <> "" then
            res <- (res + " ") + right
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        nodes <- Array.empty<System.Collections.Generic.IDictionary<string, int>>
        let mutable root: int = NIL
        root <- insert_node (root) (4)
        root <- insert_node (root) (2)
        root <- insert_node (root) (3)
        printfn "%s" (inorder (root))
        printfn "%s" (_str (get_height (root)))
        root <- del_node (root) (3)
        printfn "%s" (inorder (root))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

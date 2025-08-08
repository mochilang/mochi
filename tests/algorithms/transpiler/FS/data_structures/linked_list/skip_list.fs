// Generated 2025-08-08 11:10 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let NIL: int = 0 - 1
let MAX_LEVEL: int = 6
let P: float = 0.5
let mutable _seed: int = 1
let rec random () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        _seed <- ((((_seed * 13) + 7) % 100 + 100) % 100)
        __ret <- (float _seed) / 100.0
        raise Return
        __ret
    with
        | Return -> __ret
and random_level () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        let mutable lvl: int = 1
        while ((random()) < P) && (lvl < MAX_LEVEL) do
            lvl <- lvl + 1
        __ret <- lvl
        raise Return
        __ret
    with
        | Return -> __ret
and empty_forward () =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    try
        let mutable f: int array = [||]
        let mutable i: int = 0
        while i < MAX_LEVEL do
            f <- Array.append f [|NIL|]
            i <- i + 1
        __ret <- f
        raise Return
        __ret
    with
        | Return -> __ret
let mutable node_keys: int array = [||]
let mutable node_vals: int array = [||]
let mutable node_forwards: int array array = [||]
let mutable level: int = 1
let rec init () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        node_keys <- unbox<int array> [|-1|]
        node_vals <- unbox<int array> [|0|]
        node_forwards <- [|empty_forward()|]
        level <- 1
        __ret
    with
        | Return -> __ret
and insert (key: int) (value: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable key = key
    let mutable value = value
    try
        let mutable update: int array = [||]
        let mutable i: int = 0
        while i < MAX_LEVEL do
            update <- Array.append update [|0|]
            i <- i + 1
        let mutable x: int = 0
        i <- level - 1
        while i >= 0 do
            while ((_idx (_idx node_forwards (x)) (i)) <> NIL) && ((_idx node_keys (_idx (_idx node_forwards (x)) (i))) < key) do
                x <- _idx (_idx node_forwards (x)) (i)
            update.[i] <- x
            i <- i - 1
        x <- _idx (_idx node_forwards (x)) (0)
        if (x <> NIL) && ((_idx node_keys (x)) = key) then
            node_vals <- _arrset node_vals (x) (value)
            __ret <- ()
            raise Return
        let mutable lvl: int = random_level()
        if lvl > level then
            let mutable j: int = level
            while j < lvl do
                update.[j] <- 0
                j <- j + 1
            level <- lvl
        node_keys <- Array.append node_keys [|key|]
        node_vals <- Array.append node_vals [|value|]
        let mutable forwards: int array = empty_forward()
        let idx: int = (Seq.length (node_keys)) - 1
        i <- 0
        while i < lvl do
            forwards.[i] <- _idx (_idx node_forwards (_idx update (i))) (i)
            node_forwards.[_idx update (i)].[i] <- idx
            i <- i + 1
        node_forwards <- Array.append node_forwards [|forwards|]
        __ret
    with
        | Return -> __ret
and find (key: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable key = key
    try
        let mutable x: int = 0
        let mutable i: int = level - 1
        while i >= 0 do
            while ((_idx (_idx node_forwards (x)) (i)) <> NIL) && ((_idx node_keys (_idx (_idx node_forwards (x)) (i))) < key) do
                x <- _idx (_idx node_forwards (x)) (i)
            i <- i - 1
        x <- _idx (_idx node_forwards (x)) (0)
        if (x <> NIL) && ((_idx node_keys (x)) = key) then
            __ret <- _idx node_vals (x)
            raise Return
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and delete (key: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable key = key
    try
        let mutable update: int array = [||]
        let mutable i: int = 0
        while i < MAX_LEVEL do
            update <- Array.append update [|0|]
            i <- i + 1
        let mutable x: int = 0
        i <- level - 1
        while i >= 0 do
            while ((_idx (_idx node_forwards (x)) (i)) <> NIL) && ((_idx node_keys (_idx (_idx node_forwards (x)) (i))) < key) do
                x <- _idx (_idx node_forwards (x)) (i)
            update.[i] <- x
            i <- i - 1
        x <- _idx (_idx node_forwards (x)) (0)
        if (x = NIL) || ((_idx node_keys (x)) <> key) then
            __ret <- ()
            raise Return
        i <- 0
        while i < level do
            if (_idx (_idx node_forwards (_idx update (i))) (i)) = x then
                node_forwards.[_idx update (i)].[i] <- _idx (_idx node_forwards (x)) (i)
            i <- i + 1
        while (level > 1) && ((_idx (_idx node_forwards (0)) (level - 1)) = NIL) do
            level <- level - 1
        __ret
    with
        | Return -> __ret
and to_string () =
    let mutable __ret : string = Unchecked.defaultof<string>
    try
        let mutable s: string = ""
        let mutable x: int = _idx (_idx node_forwards (0)) (0)
        while x <> NIL do
            if s <> "" then
                s <- s + " -> "
            s <- ((s + (_str (_idx node_keys (x)))) + ":") + (_str (_idx node_vals (x)))
            x <- _idx (_idx node_forwards (x)) (0)
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        init()
        insert (2) (2)
        insert (4) (4)
        insert (6) (4)
        insert (4) (5)
        insert (8) (4)
        insert (9) (4)
        delete (4)
        printfn "%s" (to_string())
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()

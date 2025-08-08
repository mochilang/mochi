// Generated 2025-08-08 16:03 +0700

exception Break
exception Continue

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
type Pos = {
    mutable _x: int
    mutable _y: int
}
type PQNode = {
    mutable _pos: Pos
    mutable _pri: float
}
type PQPopResult = {
    mutable _pq: PQNode array
    mutable _node: PQNode
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let W1: float = 1.0
let W2: float = 1.0
let n: int = 20
let n_heuristic: int = 3
let INF: float = 1000000000.0
let mutable t: int = 1
let rec pos_equal (a: Pos) (b: Pos) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        __ret <- ((a._x) = (b._x)) && ((a._y) = (b._y))
        raise Return
        __ret
    with
        | Return -> __ret
let rec pos_key (p: Pos) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable p = p
    try
        __ret <- ((_str (p._x)) + ",") + (_str (p._y))
        raise Return
        __ret
    with
        | Return -> __ret
let rec sqrtApprox (_x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _x = _x
    try
        if _x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = _x
        let mutable i: int = 0
        while i < 10 do
            guess <- (guess + (_x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec consistent_heuristic (p: Pos) (goal: Pos) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable p = p
    let mutable goal = goal
    try
        let dx: float = float ((p._x) - (goal._x))
        let dy: float = float ((p._y) - (goal._y))
        __ret <- sqrtApprox ((dx * dx) + (dy * dy))
        raise Return
        __ret
    with
        | Return -> __ret
let rec iabs (_x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable _x = _x
    try
        __ret <- if _x < 0 then (-_x) else _x
        raise Return
        __ret
    with
        | Return -> __ret
let rec heuristic_1 (p: Pos) (goal: Pos) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable p = p
    let mutable goal = goal
    try
        __ret <- float ((iabs ((p._x) - (goal._x))) + (iabs ((p._y) - (goal._y))))
        raise Return
        __ret
    with
        | Return -> __ret
let rec heuristic_2 (p: Pos) (goal: Pos) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable p = p
    let mutable goal = goal
    try
        let h: float = consistent_heuristic (p) (goal)
        __ret <- h / (float t)
        raise Return
        __ret
    with
        | Return -> __ret
let rec heuristic (i: int) (p: Pos) (goal: Pos) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable i = i
    let mutable p = p
    let mutable goal = goal
    try
        if i = 0 then
            __ret <- consistent_heuristic (p) (goal)
            raise Return
        if i = 1 then
            __ret <- heuristic_1 (p) (goal)
            raise Return
        __ret <- heuristic_2 (p) (goal)
        raise Return
        __ret
    with
        | Return -> __ret
let rec key_fn (start: Pos) (i: int) (goal: Pos) (g_func: System.Collections.Generic.IDictionary<string, float>) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable start = start
    let mutable i = i
    let mutable goal = goal
    let mutable g_func = g_func
    try
        let g: float = _dictGet g_func ((string (pos_key (start))))
        __ret <- g + (W1 * (heuristic (i) (start) (goal)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec valid (p: Pos) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable p = p
    try
        if ((p._x) < 0) || ((p._x) > (n - 1)) then
            __ret <- false
            raise Return
        if ((p._y) < 0) || ((p._y) > (n - 1)) then
            __ret <- false
            raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let blocks: Pos array = [|{ _x = 0; _y = 1 }; { _x = 1; _y = 1 }; { _x = 2; _y = 1 }; { _x = 3; _y = 1 }; { _x = 4; _y = 1 }; { _x = 5; _y = 1 }; { _x = 6; _y = 1 }; { _x = 7; _y = 1 }; { _x = 8; _y = 1 }; { _x = 9; _y = 1 }; { _x = 10; _y = 1 }; { _x = 11; _y = 1 }; { _x = 12; _y = 1 }; { _x = 13; _y = 1 }; { _x = 14; _y = 1 }; { _x = 15; _y = 1 }; { _x = 16; _y = 1 }; { _x = 17; _y = 1 }; { _x = 18; _y = 1 }; { _x = 19; _y = 1 }|]
let rec in_blocks (p: Pos) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable p = p
    try
        let mutable i: int = 0
        while i < (Seq.length (blocks)) do
            if pos_equal (_idx blocks (i)) (p) then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec pq_put (_pq: PQNode array) (_node: Pos) (_pri: float) =
    let mutable __ret : PQNode array = Unchecked.defaultof<PQNode array>
    let mutable _pq = _pq
    let mutable _node = _node
    let mutable _pri = _pri
    try
        let mutable updated: bool = false
        let mutable i: int = 0
        while i < (Seq.length (_pq)) do
            if pos_equal ((_idx _pq (i))._pos) (_node) then
                if _pri < ((_idx _pq (i))._pri) then
                    _pq.[i] <- { _pos = _node; _pri = _pri }
                updated <- true
            i <- i + 1
        if not updated then
            _pq <- Array.append _pq [|{ _pos = _node; _pri = _pri }|]
        __ret <- _pq
        raise Return
        __ret
    with
        | Return -> __ret
let rec pq_minkey (_pq: PQNode array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _pq = _pq
    try
        if (Seq.length (_pq)) = 0 then
            __ret <- INF
            raise Return
        let mutable first: PQNode = _idx _pq (0)
        let mutable m: float = first._pri
        let mutable i: int = 1
        while i < (Seq.length (_pq)) do
            let mutable item: PQNode = _idx _pq (i)
            if (item._pri) < m then
                m <- item._pri
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
let rec pq_pop_min (_pq: PQNode array) =
    let mutable __ret : PQPopResult = Unchecked.defaultof<PQPopResult>
    let mutable _pq = _pq
    try
        let mutable best: PQNode = _idx _pq (0)
        let mutable idx: int = 0
        let mutable i: int = 1
        while i < (Seq.length (_pq)) do
            if ((_idx _pq (i))._pri) < (best._pri) then
                best <- _idx _pq (i)
                idx <- i
            i <- i + 1
        let mutable new_pq: PQNode array = [||]
        i <- 0
        while i < (Seq.length (_pq)) do
            if i <> idx then
                new_pq <- Array.append new_pq [|(_idx _pq (i))|]
            i <- i + 1
        __ret <- { _pq = new_pq; _node = best }
        raise Return
        __ret
    with
        | Return -> __ret
let rec pq_remove (_pq: PQNode array) (_node: Pos) =
    let mutable __ret : PQNode array = Unchecked.defaultof<PQNode array>
    let mutable _pq = _pq
    let mutable _node = _node
    try
        let mutable new_pq: PQNode array = [||]
        let mutable i: int = 0
        while i < (Seq.length (_pq)) do
            if not (pos_equal ((_idx _pq (i))._pos) (_node)) then
                new_pq <- Array.append new_pq [|(_idx _pq (i))|]
            i <- i + 1
        __ret <- new_pq
        raise Return
        __ret
    with
        | Return -> __ret
let rec reconstruct (back_pointer: System.Collections.Generic.IDictionary<string, Pos>) (goal: Pos) (start: Pos) =
    let mutable __ret : Pos array = Unchecked.defaultof<Pos array>
    let mutable back_pointer = back_pointer
    let mutable goal = goal
    let mutable start = start
    try
        let mutable path: Pos array = [||]
        let mutable current: Pos = goal
        let mutable key: string = pos_key (current)
        path <- Array.append path [|current|]
        while not (pos_equal (current) (start)) do
            current <- _dictGet back_pointer ((string (key)))
            key <- pos_key (current)
            path <- Array.append path [|current|]
        let mutable rev: Pos array = [||]
        let mutable i: int = (Seq.length (path)) - 1
        while i >= 0 do
            rev <- Array.append rev [|(_idx path (i))|]
            i <- i - 1
        __ret <- rev
        raise Return
        __ret
    with
        | Return -> __ret
let rec neighbours (p: Pos) =
    let mutable __ret : Pos array = Unchecked.defaultof<Pos array>
    let mutable p = p
    try
        let left: Pos = { _x = (p._x) - 1; _y = p._y }
        let right: Pos = { _x = (p._x) + 1; _y = p._y }
        let up: Pos = { _x = p._x; _y = (p._y) + 1 }
        let down: Pos = { _x = p._x; _y = (p._y) - 1 }
        __ret <- unbox<Pos array> [|left; right; up; down|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec multi_a_star (start: Pos) (goal: Pos) (n_heuristic: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable start = start
    let mutable goal = goal
    let mutable n_heuristic = n_heuristic
    try
        let mutable g_function: System.Collections.Generic.IDictionary<string, float> = _dictCreate []
        let mutable back_pointer: System.Collections.Generic.IDictionary<string, Pos> = _dictCreate []
        let mutable visited: System.Collections.Generic.IDictionary<string, bool> = _dictCreate []
        let mutable open_list: PQNode array array = [||]
        g_function.[pos_key (start)] <- 0.0
        g_function.[pos_key (goal)] <- INF
        back_pointer.[pos_key (start)] <- { _x = -1; _y = -1 }
        back_pointer.[pos_key (goal)] <- { _x = -1; _y = -1 }
        visited.[pos_key (start)] <- true
        let mutable i: int = 0
        while i < n_heuristic do
            open_list <- Array.append open_list [|[||]|]
            let _pri: float = key_fn (start) (i) (goal) (g_function)
            open_list.[i] <- pq_put (_idx open_list (i)) (start) (_pri)
            i <- i + 1
        try
            while (pq_minkey (_idx open_list (0))) < INF do
                try
                    let mutable chosen: int = 0
                    i <- 1
                    try
                        while i < n_heuristic do
                            try
                                if (pq_minkey (_idx open_list (i))) <= (W2 * (pq_minkey (_idx open_list (0)))) then
                                    chosen <- i
                                    raise Break
                                i <- i + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if chosen <> 0 then
                        t <- t + 1
                    let mutable pair: PQPopResult = pq_pop_min (_idx open_list (chosen))
                    open_list.[chosen] <- pair._pq
                    let mutable current: PQNode = pair._node
                    i <- 0
                    while i < n_heuristic do
                        if i <> chosen then
                            open_list.[i] <- pq_remove (_idx open_list (i)) (current._pos)
                        i <- i + 1
                    let ckey: string = pos_key (current._pos)
                    if visited.ContainsKey(ckey) then
                        raise Continue
                    visited.[ckey] <- true
                    if pos_equal (current._pos) (goal) then
                        let mutable path: Pos array = reconstruct (back_pointer) (goal) (start)
                        let mutable j: int = 0
                        while j < (Seq.length (path)) do
                            let p: Pos = _idx path (j)
                            printfn "%s" (((("(" + (_str (p._x))) + ",") + (_str (p._y))) + ")")
                            j <- j + 1
                        __ret <- ()
                        raise Return
                    let neighs: Pos array = neighbours (current._pos)
                    let mutable k: int = 0
                    while k < (Seq.length (neighs)) do
                        let nb: Pos = _idx neighs (k)
                        if (valid (nb)) && ((in_blocks (nb)) = false) then
                            let nkey: string = pos_key (nb)
                            let tentative: float = (_dictGet g_function ((string (ckey)))) + 1.0
                            if (not (g_function.ContainsKey(nkey))) || (tentative < (_dictGet g_function ((string (nkey))))) then
                                g_function.[nkey] <- tentative
                                back_pointer.[nkey] <- current._pos
                                i <- 0
                                while i < n_heuristic do
                                    let pri2: float = tentative + (W1 * (heuristic (i) (nb) (goal)))
                                    open_list.[i] <- pq_put (_idx open_list (i)) (nb) (pri2)
                                    i <- i + 1
                        k <- k + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        printfn "%s" ("No path found to goal")
        __ret
    with
        | Return -> __ret
let start: Pos = { _x = 0; _y = 0 }
let goal: Pos = { _x = n - 1; _y = n - 1 }
multi_a_star (start) (goal) (n_heuristic)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

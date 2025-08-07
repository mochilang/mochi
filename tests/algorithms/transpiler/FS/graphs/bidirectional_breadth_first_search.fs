// Generated 2025-08-07 16:27 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Node = {
    pos: string
    path: string array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let grid: int array array = [|[|0; 0; 0; 0; 0; 0; 0|]; [|0; 1; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 1; 0; 0; 0; 0|]; [|1; 0; 1; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 1; 0; 0|]|]
let delta: int array array = [|[|-1; 0|]; [|0; -1|]; [|1; 0|]; [|0; 1|]|]
let rec key (y: int) (x: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable y = y
    let mutable x = x
    try
        __ret <- ((_str (y)) + ",") + (_str (x))
        raise Return
        __ret
    with
        | Return -> __ret
let rec parse_int (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable value: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = string (s.[i])
            value <- (value * 10) + (int c)
            i <- i + 1
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
let rec parse_key (k: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable k = k
    try
        let mutable idx: int = 0
        while (idx < (String.length (k))) && ((_substring k idx (idx + 1)) <> ",") do
            idx <- idx + 1
        let y: int = parse_int (_substring k 0 idx)
        let x: int = parse_int (_substring k (idx + 1) (String.length (k)))
        __ret <- unbox<int array> [|y; x|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec neighbors (pos: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable pos = pos
    try
        let coords: int array = parse_key (pos)
        let y: int = _idx coords (0)
        let x: int = _idx coords (1)
        let mutable res: string array = [||]
        let mutable i: int = 0
        while i < (Seq.length (delta)) do
            let ny: int = y + (_idx (_idx delta (i)) (0))
            let nx: int = x + (_idx (_idx delta (i)) (1))
            if (((ny >= 0) && (ny < (Seq.length (grid)))) && (nx >= 0)) && (nx < (Seq.length (_idx grid (0)))) then
                if (_idx (_idx grid (ny)) (nx)) = 0 then
                    res <- Array.append res [|(key (ny) (nx))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec reverse_list (lst: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable lst = lst
    try
        let mutable res: string array = [||]
        let mutable i: int = (Seq.length (lst)) - 1
        while i >= 0 do
            res <- Array.append res [|(_idx lst (i))|]
            i <- i - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec bfs (start: string) (goal: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable start = start
    let mutable goal = goal
    try
        let mutable queue: Node array = [||]
        queue <- Array.append queue [|{ pos = start; path = [|start|] }|]
        let mutable head: int = 0
        let mutable visited: System.Collections.Generic.IDictionary<string, bool> = _dictCreate [(start, true)]
        while head < (Seq.length (queue)) do
            let node: Node = _idx queue (head)
            head <- head + 1
            if (node.pos) = goal then
                __ret <- node.path
                raise Return
            let neigh: string array = neighbors (node.pos)
            let mutable i: int = 0
            while i < (Seq.length (neigh)) do
                let npos: string = _idx neigh (i)
                if not (visited.ContainsKey(npos)) then
                    visited.[npos] <- true
                    let mutable new_path: string array = Array.append (node.path) [|npos|]
                    queue <- Array.append queue [|{ pos = npos; path = new_path }|]
                i <- i + 1
        __ret <- Array.empty<string>
        raise Return
        __ret
    with
        | Return -> __ret
let rec bidirectional_bfs (start: string) (goal: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable start = start
    let mutable goal = goal
    try
        let mutable queue_f: Node array = [||]
        let mutable queue_b: Node array = [||]
        queue_f <- Array.append queue_f [|{ pos = start; path = [|start|] }|]
        queue_b <- Array.append queue_b [|{ pos = goal; path = [|goal|] }|]
        let mutable head_f: int = 0
        let mutable head_b: int = 0
        let mutable visited_f: System.Collections.Generic.IDictionary<string, string array> = _dictCreate [(start, [|start|])]
        let mutable visited_b: System.Collections.Generic.IDictionary<string, string array> = _dictCreate [(goal, [|goal|])]
        while (head_f < (Seq.length (queue_f))) && (head_b < (Seq.length (queue_b))) do
            let node_f: Node = _idx queue_f (head_f)
            head_f <- head_f + 1
            let neigh_f: string array = neighbors (node_f.pos)
            let mutable i: int = 0
            while i < (Seq.length (neigh_f)) do
                let npos: string = _idx neigh_f (i)
                if not (visited_f.ContainsKey(npos)) then
                    let mutable new_path: string array = Array.append (node_f.path) [|npos|]
                    visited_f.[npos] <- new_path
                    if visited_b.ContainsKey(npos) then
                        let mutable rev: string array = reverse_list (visited_b.[(string (npos))])
                        let mutable j: int = 1
                        while j < (Seq.length (rev)) do
                            new_path <- Array.append new_path [|(_idx rev (j))|]
                            j <- j + 1
                        __ret <- new_path
                        raise Return
                    queue_f <- Array.append queue_f [|{ pos = npos; path = new_path }|]
                i <- i + 1
            let node_b: Node = _idx queue_b (head_b)
            head_b <- head_b + 1
            let neigh_b: string array = neighbors (node_b.pos)
            let mutable j: int = 0
            while j < (Seq.length (neigh_b)) do
                let nposb: string = _idx neigh_b (j)
                if not (visited_b.ContainsKey(nposb)) then
                    let mutable new_path_b: string array = Array.append (node_b.path) [|nposb|]
                    visited_b.[nposb] <- new_path_b
                    if visited_f.ContainsKey(nposb) then
                        let mutable path_f: string array = visited_f.[(string (nposb))]
                        new_path_b <- reverse_list (new_path_b)
                        let mutable t: int = 1
                        while t < (Seq.length (new_path_b)) do
                            path_f <- Array.append path_f [|(_idx new_path_b (t))|]
                            t <- t + 1
                        __ret <- path_f
                        raise Return
                    queue_b <- Array.append queue_b [|{ pos = nposb; path = new_path_b }|]
                j <- j + 1
        __ret <- unbox<string array> [|start|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec path_to_string (path: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable path = path
    try
        if (Seq.length (path)) = 0 then
            __ret <- "[]"
            raise Return
        let mutable first: int array = parse_key (_idx path (0))
        let mutable s: string = ((("[(" + (_str (_idx first (0)))) + ", ") + (_str (_idx first (1)))) + ")"
        let mutable i: int = 1
        while i < (Seq.length (path)) do
            let c: int array = parse_key (_idx path (i))
            s <- ((((s + ", (") + (_str (_idx c (0)))) + ", ") + (_str (_idx c (1)))) + ")"
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let start: string = key (0) (0)
let goal: string = key ((Seq.length (grid)) - 1) ((Seq.length (_idx grid (0))) - 1)
let path1: string array = bfs (start) (goal)
printfn "%s" (path_to_string (path1))
let path2: string array = bidirectional_bfs (start) (goal)
printfn "%s" (path_to_string (path2))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

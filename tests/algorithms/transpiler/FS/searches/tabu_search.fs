// Generated 2025-08-11 16:20 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Solution = {
    mutable _path: string array
    mutable _cost: int
}
type Swap = {
    mutable _a: string
    mutable _b: string
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec path_cost (_path: string array) (graph: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, int>>) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable _path = _path
    let mutable graph = graph
    try
        let mutable total: int = 0
        let mutable i: int = 0
        while i < ((Seq.length (_path)) - 1) do
            let u: string = _idx _path (int i)
            let v: string = _idx _path (int (i + 1))
            total <- int (total + (int (_dictGet graph (u).[v])))
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
let rec generate_first_solution (graph: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, int>>) (start: string) =
    let mutable __ret : Solution = Unchecked.defaultof<Solution>
    let mutable graph = graph
    let mutable start = start
    try
        let mutable _path: string array = Array.empty<string>
        let mutable visiting: string = start
        let mutable total: int = 0
        try
            while (Seq.length (_path)) < (Seq.length (graph)) do
                try
                    _path <- Array.append _path [|visiting|]
                    let mutable best_node: string = ""
                    let mutable best_cost: int = 1000000
                    for n in _dictGet graph (visiting) do
                        if (not (Seq.contains n _path)) && ((int (_dictGet graph (visiting).[n])) < best_cost) then
                            best_cost <- int (_dictGet graph (visiting).[n])
                            best_node <- unbox<string> n
                    if best_node = "" then
                        raise Break
                    total <- total + best_cost
                    visiting <- best_node
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        _path <- Array.append _path [|start|]
        total <- int (total + (int (_dictGet graph (visiting).[start])))
        __ret <- { _path = _path; _cost = total }
        raise Return
        __ret
    with
        | Return -> __ret
let rec copy_path (_path: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable _path = _path
    try
        let mutable res: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (Seq.length (_path)) do
            res <- Array.append res [|(_idx _path (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_neighborhood (sol: Solution) (graph: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, int>>) =
    let mutable __ret : Solution array = Unchecked.defaultof<Solution array>
    let mutable sol = sol
    let mutable graph = graph
    try
        let mutable neighbors: Solution array = Array.empty<Solution>
        let mutable i: int = 1
        while i < ((Seq.length (sol._path)) - 1) do
            let mutable j: int = 1
            while j < ((Seq.length (sol._path)) - 1) do
                if i <> j then
                    let mutable new_path: string array = copy_path (sol._path)
                    let tmp: string = _idx new_path (int i)
                    new_path.[int i] <- _idx new_path (int j)
                    new_path.[int j] <- tmp
                    let _cost: int = path_cost (new_path) (graph)
                    neighbors <- Array.append neighbors [|{ _path = new_path; _cost = _cost }|]
                j <- j + 1
            i <- i + 1
        __ret <- neighbors
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_swap (_a: string array) (_b: string array) =
    let mutable __ret : Swap = Unchecked.defaultof<Swap>
    let mutable _a = _a
    let mutable _b = _b
    try
        let mutable i: int = 0
        while i < (Seq.length (_a)) do
            if (_idx _a (int i)) <> (_idx _b (int i)) then
                __ret <- { _a = _idx _a (int i); _b = _idx _b (int i) }
                raise Return
            i <- i + 1
        __ret <- { _a = ""; _b = "" }
        raise Return
        __ret
    with
        | Return -> __ret
let rec tabu_search (first: Solution) (graph: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, int>>) (iters: int) (size: int) =
    let mutable __ret : Solution = Unchecked.defaultof<Solution>
    let mutable first = first
    let mutable graph = graph
    let mutable iters = iters
    let mutable size = size
    try
        let mutable solution: Solution = first
        let mutable best: Solution = first
        let mutable tabu: Swap array = Array.empty<Swap>
        let mutable count: int = 0
        try
            while count < iters do
                try
                    let neighborhood: Solution array = find_neighborhood (solution) (graph)
                    if (Seq.length (neighborhood)) = 0 then
                        raise Break
                    let mutable best_neighbor: Solution = _idx neighborhood (int 0)
                    let mutable best_move: Swap = find_swap (solution._path) (best_neighbor._path)
                    let mutable i: int = 1
                    while i < (Seq.length (neighborhood)) do
                        let cand: Solution = _idx neighborhood (int i)
                        let move: Swap = find_swap (solution._path) (cand._path)
                        let mutable forbidden: bool = false
                        let mutable t: int = 0
                        while t < (Seq.length (tabu)) do
                            if ((((_idx tabu (int t))._a) = (move._a)) && (((_idx tabu (int t))._b) = (move._b))) || ((((_idx tabu (int t))._a) = (move._b)) && (((_idx tabu (int t))._b) = (move._a))) then
                                forbidden <- true
                            t <- t + 1
                        if (forbidden = false) && ((cand._cost) < (best_neighbor._cost)) then
                            best_neighbor <- cand
                            best_move <- move
                        i <- i + 1
                    solution <- best_neighbor
                    tabu <- Array.append tabu [|best_move|]
                    if (Seq.length (tabu)) > size then
                        let mutable new_tab: Swap array = Array.empty<Swap>
                        let mutable j: int = 1
                        while j < (Seq.length (tabu)) do
                            new_tab <- Array.append new_tab [|(_idx tabu (int j))|]
                            j <- j + 1
                        tabu <- new_tab
                    if (solution._cost) < (best._cost) then
                        best <- solution
                    count <- count + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- best
        raise Return
        __ret
    with
        | Return -> __ret
let graph: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, int>> = unbox<System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, int>>> (_dictCreate [("a", _dictCreate [("b", 20); ("c", 18); ("d", 22); ("e", 26)]); ("b", _dictCreate [("a", 20); ("c", 10); ("d", 11); ("e", 12)]); ("c", _dictCreate [("a", 18); ("b", 10); ("d", 23); ("e", 24)]); ("d", _dictCreate [("a", 22); ("b", 11); ("c", 23); ("e", 40)]); ("e", _dictCreate [("a", 26); ("b", 12); ("c", 24); ("d", 40)])])
let first: Solution = generate_first_solution (graph) ("a")
let mutable best: Solution = tabu_search (first) (graph) (4) (3)
printfn "%s" (_str (best._path))
printfn "%s" (_str (best._cost))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

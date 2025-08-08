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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Point = {
    mutable _x: int
    mutable _y: int
}
type Result = {
    mutable _distance: float
    mutable _path: Point array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec key (p: Point) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable p = p
    try
        __ret <- ((_str (p._x)) + ",") + (_str (p._y))
        raise Return
        __ret
    with
        | Return -> __ret
let rec path_to_string (_path: Point array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable _path = _path
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (_path)) do
            let pt: Point = _idx _path (i)
            s <- ((((s + "(") + (_str (pt._x))) + ", ") + (_str (pt._y))) + ")"
            if i < ((Seq.length (_path)) - 1) then
                s <- s + ", "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec dijkstra (grid: int array array) (source: Point) (destination: Point) (allow_diagonal: bool) =
    let mutable __ret : Result = Unchecked.defaultof<Result>
    let mutable grid = grid
    let mutable source = source
    let mutable destination = destination
    let mutable allow_diagonal = allow_diagonal
    try
        let rows: int = Seq.length (grid)
        let cols: int = Seq.length (_idx grid (0))
        let mutable dx: int array = [|-1; 1; 0; 0|]
        let mutable dy: int array = [|0; 0; -1; 1|]
        if allow_diagonal then
            dx <- unbox<int array> (Array.append (dx) ([|-1; -1; 1; 1|]))
            dy <- unbox<int array> (Array.append (dy) ([|-1; 1; -1; 1|]))
        let INF: float = 1000000000000.0
        let mutable queue: Point array = [|source|]
        let mutable _front: int = 0
        let mutable dist_map: System.Collections.Generic.IDictionary<string, float> = _dictCreate [(key (source), 0.0)]
        let mutable prev: System.Collections.Generic.IDictionary<string, Point> = _dictCreate []
        try
            while _front < (Seq.length (queue)) do
                try
                    let current: Point = _idx queue (_front)
                    _front <- _front + 1
                    let cur_key: string = key (current)
                    if ((current._x) = (destination._x)) && ((current._y) = (destination._y)) then
                        raise Break
                    let mutable i: int = 0
                    while i < (Seq.length (dx)) do
                        let nx: int = (current._x) + (_idx dx (i))
                        let ny: int = (current._y) + (_idx dy (i))
                        if (((nx >= 0) && (nx < rows)) && (ny >= 0)) && (ny < cols) then
                            if (_idx (_idx grid (nx)) (ny)) = 1 then
                                let n_key: string = ((_str (nx)) + ",") + (_str (ny))
                                if not (dist_map.ContainsKey(n_key)) then
                                    dist_map.[n_key] <- (_dictGet dist_map ((string (cur_key)))) + 1.0
                                    prev.[n_key] <- current
                                    queue <- Array.append queue [|{ _x = nx; _y = ny }|]
                        i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let dest_key: string = key (destination)
        if dist_map.ContainsKey(dest_key) then
            let mutable path_rev: Point array = [|destination|]
            let mutable step_key: string = dest_key
            let mutable step_pt: Point = destination
            while step_key <> (key (source)) do
                step_pt <- _dictGet prev ((string (step_key)))
                step_key <- key (step_pt)
                path_rev <- Array.append path_rev [|step_pt|]
            let mutable _path: Point array = [||]
            let mutable k: int = (Seq.length (path_rev)) - 1
            while k >= 0 do
                _path <- Array.append _path [|(_idx path_rev (k))|]
                k <- k - 1
            __ret <- { _distance = _dictGet dist_map ((string (dest_key))); _path = _path }
            raise Return
        __ret <- { _distance = INF; _path = [||] }
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_result (res: Result) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable res = res
    try
        printfn "%s" (((_str (res._distance)) + ", ") + (path_to_string (res._path)))
        __ret
    with
        | Return -> __ret
let mutable grid1: int array array = [|[|1; 1; 1|]; [|0; 1; 0|]; [|0; 1; 1|]|]
print_result (dijkstra (grid1) ({ _x = 0; _y = 0 }) ({ _x = 2; _y = 2 }) (false))
print_result (dijkstra (grid1) ({ _x = 0; _y = 0 }) ({ _x = 2; _y = 2 }) (true))
let mutable grid2: int array array = [|[|1; 1; 1|]; [|0; 0; 1|]; [|0; 1; 1|]|]
print_result (dijkstra (grid2) ({ _x = 0; _y = 0 }) ({ _x = 2; _y = 2 }) (false))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
